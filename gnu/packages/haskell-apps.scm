;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2019, 2020 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2020 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2021 EuAndreh <eu@euandre.org>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
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

(define-module (gnu packages haskell-apps)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg))

(define-public apply-refact
  (package
    (name "apply-refact")
    (version "0.6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/apply-refact/apply-refact-"
             version ".tar.gz"))
       (sha256
        (base32
         "0p2mqgjcqr1zcmk8zgr0yq7g8z1agsa6l493lkj6g3ya9lyhxgig"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-refact" ,ghc-refact)
       ("ghc-exactprint" ,ghc-exactprint)
       ("ghc-syb" ,ghc-syb)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-filemanip" ,ghc-filemanip)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-optparse-applicative"
        ,ghc-optparse-applicative)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-golden" ,ghc-tasty-golden)
       ("ghc-tasty-expected-failure"
        ,ghc-tasty-expected-failure)
       ("ghc-silently" ,ghc-silently)))
    (home-page "https://hackage.haskell.org/package/apply-refact")
    (synopsis "Perform refactorings specified by the refact library")
    (description
     "This package lets you perform refactorings specified by the refact
library.  It is primarily used with HLint's @code{--refactor} flag.")
    (license license:bsd-3)))

;; In Stackage LTS 14, this package is at 2.4.1.0.  However, that
;; version requires version 2.4.1.0 of the 'Cabal' package, which is
;; provided by GHC 8.6.5 at version 2.4.0.1.  Hence, we use an older
;; version to match the compiler's library.
(define-public cabal-install
 (package
  (name "cabal-install")
   (version "2.4.0.0")
   (source
    (origin
     (method url-fetch)
      (uri (string-append
            "https://hackage.haskell.org/package/cabal-install/cabal-install-"
            version
            ".tar.gz"))
      (sha256
       (base32 "1xmyl0x8wqfrnray6ky5wy0g0samv4264fbdlzxhqsvk9dbfja8k"))))
   (build-system haskell-build-system)
   (arguments
    `(#:cabal-revision
      ("2" "1xil5pim6j1ckqj61zz6l7xpfxxr3rkw2hvpws2f7pr9shk645dl")
      #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "cabal-install.cabal"
               (("zip-archive >= 0\\.3\\.2\\.5 && < 0\\.4,")
                "zip-archive >= 0.3.2.5 && <= 0.4.1,"))
             #t)))))
   (inputs
    `(("ghc-async" ,ghc-async)
      ("ghc-base16-bytestring" ,ghc-base16-bytestring)
      ("ghc-cryptohash-sha256" ,ghc-cryptohash-sha256)
      ("ghc-echo" ,ghc-echo)
      ("ghc-edit-distance" ,ghc-edit-distance)
      ("ghc-hackage-security" ,ghc-hackage-security)
      ("ghc-hashable" ,ghc-hashable)
      ("ghc-http" ,ghc-http)
      ("ghc-network-uri" ,ghc-network-uri)
      ("ghc-network" ,ghc-network)
      ("ghc-random" ,ghc-random)
      ("ghc-resolv" ,ghc-resolv)
      ("ghc-tar" ,ghc-tar)
      ("ghc-zip-archive" ,ghc-zip-archive)
      ("ghc-zlib" ,ghc-zlib)))
   (home-page "https://www.haskell.org/cabal/")
   (synopsis "Command-line interface for Cabal and Hackage")
   (description
    "The cabal command-line program simplifies the process of managing
Haskell software by automating the fetching, configuration, compilation and
installation of Haskell libraries and programs.")
   (license license:bsd-3)))

(define-public corrode
  (let ((commit "b6699fb2fa552a07c6091276285a44133e5c9789"))
    (package
      (name "corrode")
      (version (string-append "0.0.1-" (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jameysharp/corrode")
               (commit "b6699fb2fa552a07c6091276285a44133e5c9789")))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02v0yyj6sk4gpg2222wzsdqjxn8w66scbnf6b20x0kbmc69qcz4r"))))
      (build-system haskell-build-system)
      (inputs
       `(("ghc-language-c" ,ghc-language-c)
         ("ghc-markdown-unlit" ,ghc-markdown-unlit)))
      (home-page "https://github.com/jameysharp/corrode")
      (synopsis "Automatic semantics-preserving translation from C to Rust")
      (description
       "This program reads a C source file and prints an equivalent module in
Rust syntax.  It is intended to be useful for two different purposes:

@enumerate
@item Partial automation for migrating legacy code that was implemented in C.
@item A new, complementary approach to static analysis for C programs.
@end enumerate\n")
      (license license:gpl2+))))

(define-public cpphs
  (package
    (name "cpphs")
    (version "1.20.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/" name "/"
             name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bh524asqhk9v1s0wvipl0hgn7l63iy3js867yv0z3h5v2kn8vg5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-polyparse" ,ghc-polyparse)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-old-time" ,ghc-old-time)))
    (home-page "http://projects.haskell.org/cpphs/")
    (synopsis "Liberalised re-implementation of cpp, the C pre-processor")
    (description "Cpphs is a re-implementation of the C pre-processor that is
both more compatible with Haskell, and itself written in Haskell so that it
can be distributed with compilers.  This version of the C pre-processor is
pretty-much feature-complete and compatible with traditional (K&R)
pre-processors.  Additional features include: a plain-text mode; an option to
unlit literate code files; and an option to turn off macro-expansion.")
    (license (list license:lgpl2.1+ license:gpl3+))))

;; Darcs has no https support:
;; http://darcs.net/manual/Configuring_darcs.html#SECTION00440070000000000000
;; and results of search engines will show that if the protocol is http, https
;; is never mentioned.
(define-public darcs
  (package
    (name "darcs")
    (version "2.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/darcs/"
                           "darcs-" version ".tar.gz"))
       (sha256
        (base32
         "0zm2486gyhiga1amclbg92cd09bvki6vgh0ll75hv5kl72j61lb5"))
       (modules '((guix build utils)))
       ;; Remove time-dependent code for reproducibility.
       (snippet
        '(begin
           (substitute* "darcs/darcs.hs"
             (("__DATE__") "\"1970-01-01\"")
             (("__TIME__") "\"00:00:00\""))
           #t))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "0xl7j5cm704pbl2ms0dkydh7jvrz0ym76d725ifpg4h902m1zkhg")
       #:configure-flags '("-fpkgconfig" "-fcurl" "-flibiconv" "-fthreaded"
                           "-fnetwork-uri" "-fhttp" "--flag=executable"
                           "--flag=library")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-sh
           (lambda _
             (substitute* "tests/issue538.sh"
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "darcs.cabal"
               (("QuickCheck   >= 2\\.8\\.2 && < 2\\.13,")
                "QuickCheck   >= 2.8.2 && < 2.14,"))
             #t)))))
    (inputs
     `(("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-split" ,ghc-split)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-findbin" ,ghc-findbin)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-async" ,ghc-async)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base16-bytestring" ,ghc-base16-bytestring)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-cryptohash" ,ghc-cryptohash)
       ("ghc-data-ordlist" ,ghc-data-ordlist)
       ("ghc-fgl" ,ghc-fgl)
       ("ghc-system-filepath" ,ghc-system-filepath)
       ("ghc-graphviz" ,ghc-graphviz)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-html" ,ghc-html)
       ("ghc-mmap" ,ghc-mmap)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-random" ,ghc-random)
       ("ghc-regex-applicative" ,ghc-regex-applicative)
       ("ghc-regex-compat-tdfa" ,ghc-regex-compat-tdfa)
       ("ghc-sandi" ,ghc-sandi)
       ("ghc-shelly" ,ghc-shelly)
       ("ghc-tar" ,ghc-tar)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-vector" ,ghc-vector)
       ("ghc-zip-archive" ,ghc-zip-archive)
       ("ghc-zlib" ,ghc-zlib)
       ("ghc-http" ,ghc-http)
       ("curl" ,curl)
       ("ghc" ,ghc)
       ("ncurses" ,ncurses)
       ("perl" ,perl)
       ("libiconv" ,libiconv)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://darcs.net")
    (synopsis "Distributed Revision Control System")
    (description
     "Darcs is a revision control system.  It is:

@enumerate
@item Distributed: Every user has access to the full command set, removing boundaries
between server and client or committer and non-committers.
@item Interactive: Darcs is easy to learn and efficient to use because it asks you
questions in response to simple commands, giving you choices in your work flow.
You can choose to record one change in a file, while ignoring another.  As you update
from upstream, you can review each patch name, even the full diff for interesting
patches.
@item Smart: Originally developed by physicist David Roundy, darcs is based on a
unique algebra of patches called @url{http://darcs.net/Theory,Patchtheory}.
@end enumerate")
    (license license:gpl2)))

(define-public ghcid
  (package
    (name "ghcid")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/ghcid/"
                           "ghcid-" version ".tar.gz"))
       (sha256
        (base32 "0yqc1pkfajnr56gnh43sbj50r7c3r41b2jfz07ivgl6phi4frjbq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-extra" ,ghc-extra)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-fsnotify" ,ghc-fsnotify)
       ("ghc-terminal-size" ,ghc-terminal-size)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/ndmitchell/ghcid#readme")
    (synopsis "GHCi based bare bones IDE")
    (description
     "Either \"GHCi as a daemon\" or \"GHC + a bit of an IDE\".  A very simple Haskell
development tool which shows you the errors in your project and updates them whenever
you save.  Run @code{ghcid --topmost --command=ghci}, where @code{--topmost} makes the
window on top of all others (Windows only) and @code{--command} is the command to start
GHCi on your project (defaults to @code{ghci} if you have a @file{.ghci} file, or else
to @code{cabal repl}).")
    (license license:bsd-3)))

(define-public git-annex
  (package
    (name "git-annex")
    (version "8.20210330")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "git-annex/git-annex-" version ".tar.gz"))
       (sha256
        (base32 "07dhxlmnj48drgndcplafc7xhby0w3rks68fz9wsppxan929240p"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags
       '("--flags=-Android -Webapp")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-shell-for-tests
           (lambda _
             ;; Shell.hs defines "/bin/sh" that is used in Git hooks.  We
             ;; shouldn't patch hooks with Guix's current bash because the
             ;; hooks can exist after that bash is garbage collected, but
             ;; let's temporarily patch it so that we can run the tests.
             (copy-file "Utility/Shell.hs" "/tmp/Shell.hs")
             (substitute* "Utility/Shell.hs"
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'configure 'factor-setup
           (lambda _
             ;; Factor out necessary build logic from the provided
             ;; `Setup.hs' script.  The script as-is does not work because
             ;; it cannot find its dependencies, and there is no obvious way
             ;; to tell it where to look.
             (call-with-output-file "PreConf.hs"
               (lambda (out)
                 (format out "import qualified Build.Configure as Configure~%")
                 (format out "main = Configure.run Configure.tests~%")))
             (call-with-output-file "Setup.hs"
               (lambda (out)
                 (format out "import Distribution.Simple~%")
                 (format out "main = defaultMain~%")))
             #t))
         (add-before 'configure 'pre-configure
           (lambda _
             (invoke "runhaskell" "PreConf.hs")
             #t))
         (add-after 'build 'build-manpages
           (lambda _
             ;; The Setup.hs rewrite above removed custom code for building
             ;; the man pages.  In addition to that code, git-annex's source
             ;; tree has a file that's not included in the tarball but is used
             ;; by the Makefile to build man pages.  Copy the core bits here.
             (call-with-output-file "Build/MakeMans.hs"
               (lambda (out)
                 (format out "module Main where~%")
                 (format out "import Build.Mans~%")
                 (format out "main = buildMansOrWarn~%")))
             (invoke "runhaskell" "Build/MakeMans.hs")))
         (replace 'check
           (lambda _
             ;; We need to set the path so that Git recognizes
             ;; `git annex' as a custom command.
             (setenv "PATH" (string-append (getenv "PATH") ":"
                                           (getcwd) "/dist/build/git-annex"))
             (with-directory-excursion "dist/build/git-annex"
               (symlink "git-annex" "git-annex-shell"))
             (invoke "git-annex" "test")
             #t))
         (add-after 'check 'unpatch-shell-and-rebuild
           (lambda args
             ;; Undo `patch-shell-for-tests'.
             (copy-file "/tmp/Shell.hs" "Utility/Shell.hs")
             (apply (assoc-ref %standard-phases 'build) args)))
         (add-after 'install 'install-manpages
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((man (string-append (assoc-ref outputs "out")
                                       "/man/man1/")))
               (mkdir-p man)
               (for-each (lambda (file) (install-file file man))
                         (find-files "man")))
             #t))
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-annex-shell"))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-remote-tor-annex"))
               #t)))
         (add-after 'install 'touch-static-output
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The Haskell build system adds a "static" output by
             ;; default, and there is no way to override this until
             ;; <https://issues.guix.gnu.org/41569> is fixed.  Without
             ;; this phase, the daemon complains because we do not
             ;; create the "static" output.
             (with-output-to-file (assoc-ref outputs "static")
               (lambda ()
                 (display "static output not used\n"))))))))
    (inputs
     `(("curl" ,curl)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-async" ,ghc-async)
       ("ghc-aws" ,ghc-aws)
       ("ghc-bloomfilter" ,ghc-bloomfilter)
       ("ghc-byteable" ,ghc-byteable)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-concurrent-output" ,ghc-concurrent-output)
       ("ghc-crypto-api" ,ghc-crypto-api)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-dav" ,ghc-dav)
       ("ghc-disk-free-space" ,ghc-disk-free-space)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-edit-distance" ,ghc-edit-distance)
       ("ghc-esqueleto" ,ghc-esqueleto)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-feed" ,ghc-feed)
       ("ghc-filepath-bytestring" ,ghc-filepath-bytestring)
       ("ghc-free" ,ghc-free)
       ("ghc-hinotify" ,ghc-hinotify)
       ("ghc-hslogger" ,ghc-hslogger)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-conduit" ,ghc-http-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-ifelse" ,ghc-ifelse)
       ("ghc-magic" ,ghc-magic)
       ("ghc-memory" ,ghc-memory)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-mountpoints" ,ghc-mountpoints)
       ("ghc-network" ,ghc-network)
       ("ghc-network-info" ,ghc-network-info)
       ("ghc-network-multicast" ,ghc-network-multicast)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-persistent" ,ghc-persistent)
       ("ghc-persistent-sqlite" ,ghc-persistent-sqlite)
       ("ghc-persistent-template" ,ghc-persistent-template)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-random" ,ghc-random)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-safesemaphore" ,ghc-safesemaphore)
       ("ghc-sandi" ,ghc-sandi)
       ("ghc-securemem" ,ghc-securemem)
       ("ghc-socks" ,ghc-socks)
       ("ghc-split" ,ghc-split)
       ("ghc-stm-chans" ,ghc-stm-chans)
       ("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-torrent" ,ghc-torrent)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-uuid" ,ghc-uuid)
       ("git" ,git)
       ("rsync" ,rsync)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-rerun" ,ghc-tasty-rerun)
       ("perl" ,perl)))
    (home-page "https://git-annex.branchable.com/")
    (synopsis "Manage files with Git, without checking in their contents")
    (description "This package allows managing files with Git, without
checking the file contents into Git.  It can store files in many places,
such as local hard drives and cloud storage services.  It can also be
used to keep a folder in sync between computers.")
    ;; The main author has released all his changes under AGPLv3+ as of March
    ;; 2019 (7.20190219-187-g40ecf58d4).  These are also licensed under the
    ;; original GPLv3+ license, but going forward new changes will be under
    ;; only AGPLv3+.  The other licenses below cover code written by others.
    ;; See git-annex's COPYRIGHT file for details on each file.
    (license (list license:agpl3+
                   license:gpl3+
                   license:bsd-2
                   license:expat
                   license:gpl2))))

(define-public hlint
  (package
    (name "hlint")
    (version "2.1.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/" name
             "/" name "-" version ".tar.gz"))
       (sha256
        (base32
         "16zkkpbfrd69853cdqf597fva969lirfc86b039i9zd7ghlrcayc"))))
    (build-system haskell-build-system)
    (inputs
     `(("cpphs" ,cpphs)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-vector" ,ghc-vector)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-haskell-src-exts-util" ,ghc-haskell-src-exts-util)
       ("ghc-uniplate" ,ghc-uniplate)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-extra" ,ghc-extra)
       ("ghc-refact" ,ghc-refact)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-lib-parser" ,ghc-lib-parser)
       ("hscolour" ,hscolour)))
    (home-page "https://github.com/ndmitchell/hlint")
    (synopsis "Suggest improvements for Haskell source code")
    (description "HLint reads Haskell programs and suggests changes that
hopefully make them easier to read.  HLint also makes it easy to disable
unwanted suggestions, and to add your own custom suggestions.")
    (license license:bsd-3)))

(define-public hoogle
  (package
    (name "hoogle")
    (version "5.0.17.11")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/hoogle/hoogle-"
         version ".tar.gz"))
       (sha256
        (base32
         "1svp8z9pad8z2j386pr0dda0ds8ddxab0salnz4gm51q877w93p1"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-connection" ,ghc-connection)
       ("ghc-extra" ,ghc-extra)
       ("ghc-foundation" ,ghc-foundation)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-http-conduit" ,ghc-http-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-js-flot" ,ghc-js-flot)
       ("ghc-js-jquery" ,ghc-js-jquery)
       ("ghc-mmap" ,ghc-mmap)
       ("ghc-process-extras" ,ghc-process-extras)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-storable-tuple" ,ghc-storable-tuple)
       ("ghc-tar" ,ghc-tar)
       ("ghc-uniplate" ,ghc-uniplate)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-vector" ,ghc-vector)
       ("ghc-wai" ,ghc-wai)
       ("ghc-wai-logger" ,ghc-wai-logger)
       ("ghc-warp" ,ghc-warp)
       ("ghc-warp-tls" ,ghc-warp-tls)
       ("ghc-zlib" ,ghc-zlib)))
    (home-page "https://hoogle.haskell.org/")
    (synopsis "Haskell API Search")
    (description "Hoogle is a Haskell API search engine, which allows
you to search many standard Haskell libraries by either function name,
or by approximate type signature.")
    (license license:bsd-3)))

(define-public hscolour
  (package
    (name "hscolour")
    (version "1.24.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hscolour/hscolour-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "079jwph4bwllfp03yfr26s5zc6m6kw3nhb1cggrifh99haq34cr4"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/hscolour")
    (synopsis "Script to colourise Haskell code")
    (description "HSColour is a small Haskell script to colourise Haskell
code.  It currently has six output formats: ANSI terminal codes (optionally
XTerm-256colour codes), HTML 3.2 with font tags, HTML 4.01 with CSS, HTML 4.01
with CSS and mouseover annotations, XHTML 1.0 with inline CSS styling, LaTeX,
and mIRC chat codes.")
    (license license:bsd-3)))

(define-public kmonad
  (package
    (name "kmonad")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/david-janssen/kmonad")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rp880zxvrznx0y1k464wjrds441dpsz94syhrkaw5dnmxf74yjd"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'haddock)             ; Haddock fails to generate docs
         (add-after 'install 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (rules (string-append out "/lib/udev/rules.d")))
               (mkdir-p rules)
               (call-with-output-file (string-append rules "/70-kmonad.rules")
                 (lambda (port)
                   (display
                    (string-append
                     "KERNEL==\"uinput\", MODE=\"0660\", "
                     "GROUP=\"input\", OPTIONS+=\"static_node=uinput\"\n")
                    port)))
               #t)))
         (add-after 'install-udev-rules 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/kmonad-" ,version)))
               (install-file "README.md" doc)
               (copy-recursively "doc" doc)
               (copy-recursively "keymap" (string-append doc "/keymap"))
               #t))))))
    (inputs
     `(("ghc-cereal" ,ghc-cereal)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-lens" ,ghc-lens)
       ("ghc-megaparsec" ,ghc-megaparsec)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-rio" ,ghc-rio)
       ("ghc-unagi-chan" ,ghc-unagi-chan)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (home-page "https://github.com/david-janssen/kmonad")
    (synopsis "Advanced keyboard manager")
    (description "KMonad is a keyboard remapping utility that supports
advanced functionality, such as custom keymap layers and modifiers, macros,
and conditional mappings that send a different keycode when tapped or held.
By operating at a lower level than most similar tools, it supports X11,
Wayland, and Linux console environments alike.")
    (license license:expat)))

(define-public nixfmt
  (package
    (name "nixfmt")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/nixfmt/nixfmt-"
             version
             ".tar.gz"))
       (sha256
        (base32 "1ispgl8rc2scr6v8bb6sks7px856jf61x74zj2iyddrn5qamkb3n"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-megaparsec" ,ghc-megaparsec)
       ("ghc-parser-combinators" ,ghc-parser-combinators)
       ("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-safe-exceptions" ,ghc-safe-exceptions)))
    (arguments
     `(#:cabal-revision
       ("1" "1hsj0jh6siph3afd9c2wii09sffl48rzqv653n4clpd8qy0rn48d")))
    (home-page "https://github.com/serokell/nixfmt")
    (synopsis "Opinionated formatter for Nix")
    (description
     "Nixfmt is a formatter for Nix that ensures consistent and clear
formatting by forgetting all existing formatting during parsing.")
    (license license:mpl2.0)))

(define-public greenclip
  (package
    (name "greenclip")
    (version "3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/erebe/greenclip")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1baw360dcnyavacf7a8v6wq4m5g6bcmyybkckv4cz7r4xl5p3qws"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxscrnsaver" ,libxscrnsaver)
       ("ghc-x11" ,ghc-x11)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-microlens" ,ghc-microlens)
       ("ghc-microlens-mtl" ,ghc-microlens-mtl)
       ("ghc-protolude" ,ghc-protolude-0.3)
       ("ghc-vector" ,ghc-vector)
       ("ghc-wordexp" ,ghc-wordexp)))
    (home-page "https://github.com/erebe/greenclip")
    (synopsis "Simple Clipboard manager")
    (description "@code{greenclip} is a clipboard manager written in
Haskell.")
    (license license:bsd-3)))

(define-public raincat
  (package
    (name "raincat")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://hackage/package/Raincat/"
                           "Raincat-" version ".tar.gz"))
       (sha256
        (base32
         "10y9zi22m6hf13c9h8zd9vg7mljpwbw0r3djb6r80bna701fdf6c"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/raincat")
                 `("LD_LIBRARY_PATH" ":" =
                   (,(string-append (assoc-ref inputs "freeglut")
                                    "/lib"))))
               #t))))))
    (inputs
     `(("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
       ("ghc-random" ,ghc-random)
       ("ghc-glut" ,ghc-glut)
       ("freeglut" ,freeglut)
       ("ghc-opengl" ,ghc-opengl)
       ("ghc-sdl2" ,ghc-sdl2)
       ("ghc-sdl2-image" ,ghc-sdl2-image)
       ("ghc-sdl2-mixer" ,ghc-sdl2-mixer)))
    (home-page "https://www.gamecreation.org/games/raincat")
    (synopsis "Puzzle game with a cat in lead role")
    (description "Project Raincat is a game developed by Carnegie Mellon
students through GCS during the Fall 2008 semester.  Raincat features game
play inspired from classics Lemmings and The Incredible Machine.  The project
proved to be an excellent learning experience for the programmers.  Everything
is programmed in Haskell.")
    (license license:bsd-3)))

(define-public scroll
  (package
    (name "scroll")
    (version "1.20180421")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/scroll/scroll-"
               version ".tar.gz"))
        (sha256
         (base32
          "0apzrvf99rskj4dbmn57jjxrsf19j436s8a09m950df5aws3a0wj"))))
    (build-system haskell-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'touch-static-output
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The Haskell build system adds a "static" output by
             ;; default, and there is no way to override this until
             ;; <https://issues.guix.gnu.org/41569> is fixed.  Without
             ;; this phase, the daemon complains because we do not
             ;; create the "static" output.
             (with-output-to-file (assoc-ref outputs "static")
               (lambda ()
                 (display "static output not used\n")))
             #t)))))
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-ifelse" ,ghc-ifelse)
       ("ghc-monad-loops" ,ghc-monad-loops)
       ("ghc-ncurses" ,ghc-ncurses)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-random" ,ghc-random)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://joeyh.name/code/scroll/")
    (synopsis "scroll(6), a roguelike game")
    (description
     "You're a bookworm that's stuck on a scroll.  You have to dodge between
words and use spells to make your way down the page as the scroll is read.  Go
too slow and you'll get wound up in the scroll and crushed.")
    (license license:gpl2)))

(define-public shellcheck
  (package
    (name "shellcheck")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/ShellCheck/ShellCheck-"
             version ".tar.gz"))
       (sha256
        (base32 "06m4wh891nah3y0br4wh3adpsb16zawkb2ijgf1vcz61fznj6ps1"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-diff" ,ghc-diff)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)))
    (home-page "https://www.shellcheck.net/")
    (synopsis "Static analysis for shell scripts")
    (description "@code{shellcheck} provides static analysis for
@command{bash} and @command{sh} shell scripts.
It gives warnings and suggestions in order to:

@enumerate
@item Point out and clarify typical beginner's syntax issues that cause
a shell to give cryptic error messages.
@item Point out and clarify typical intermediate level semantic problems
that cause a shell to behave strangely and counter-intuitively.
@item Point out subtle caveats, corner cases and pitfalls that may cause an
advanced user's otherwise working script to fail under future circumstances.
@end enumerate")
    (license license:gpl3+)))

(define-public shelltestrunner
  (package
    (name "shelltestrunner")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://hackage/package/shelltestrunner-"
                                  version "/shelltestrunner-" version ".tar.gz"))
              (sha256
               (base32
                "1a5kzqbwg6990249ypw0cx6cqj6663as1kbj8nzblcky8j6kbi6b"))))
    (build-system haskell-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs tests? parallel-tests? #:allow-other-keys)
             ;; This test is inspired by the Makefile in the upstream
             ;; repository, which is missing in the Hackage release tarball
             ;; along with some of the tests.  The Makefile would not work
             ;; anyway as it ties into the 'stack' build tool.
             (let* ((out (assoc-ref outputs "out"))
                    (shelltest (string-append out "/bin/shelltest"))
                    (numjobs (if parallel-tests?
                                 (number->string (parallel-job-count))
                                 "1")))
               (if tests?
                   (invoke shelltest (string-append "-j" numjobs)
                           "tests/examples")
                   (format #t "test suite not run~%"))
               #t))))))
    (inputs
     `(("ghc-diff" ,ghc-diff)
       ("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-filemanip" ,ghc-filemanip)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-pretty-show" ,ghc-pretty-show)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-safe" ,ghc-safe)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/simonmichael/shelltestrunner")
    (synopsis "Test CLI programs")
    (description
     "shelltestrunner (executable: @command{shelltest}) is a command-line tool
for testing command-line programs, or general shell commands.  It reads simple
test specifications defining a command to run, some input, and the expected
output, stderr, and exit status.")
    (license license:gpl3+)))

(define-public stylish-haskell
  (package
    (name "stylish-haskell")
    (version "0.9.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://hackage/package/stylish-haskell/"
         "stylish-haskell-" version ".tar.gz"))
       (sha256
        (base32
         "1zs624xqp6j8vrl6pfv18dm8vz8hvz25grri65ximxhcizgwhnax"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-file-embed" ,ghc-file-embed)
       ("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-syb" ,ghc-syb)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-strict" ,ghc-strict)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/jaspervdj/stylish-haskell")
    (synopsis "Haskell code prettifier")
    (description "Stylish-haskell is a Haskell code prettifier.  The goal is
not to format all of the code in a file, to avoid \"getting in the way\".
However, this tool can e.g. clean up import statements and help doing various
tasks that get tedious very quickly.  It can
@itemize
@item
Align and sort @code{import} statements
@item
Group and wrap @code{{-# LANGUAGE #-}} pragmas, remove (some) redundant
pragmas
@item
Remove trailing whitespaces
@item
Align branches in @code{case} and fields in records
@item
Convert line endings (customisable)
@item
Replace tabs by four spaces (turned off by default)
@item
Replace some ASCII sequences by their Unicode equivalent (turned off by
default)
@end itemize")
    (license license:bsd-3)))

(define-public ghc-stylish-haskell
  (deprecated-package "ghc-stylish-haskell" stylish-haskell))
