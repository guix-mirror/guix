;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
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
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages version-control))

;; Darcs has no https support: http://irclog.perlgeek.de/darcs/2016-09-17
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
     `(#:configure-flags '("-fpkgconfig" "-fcurl" "-flibiconv" "-fthreaded"
                           "-fnetwork-uri" "-fhttp" "--flag=executable"
                           "--flag=library")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-sh
           (lambda _
             (substitute* "tests/issue538.sh"
               (("/bin/sh") (which "sh")))
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
       ("ghc-parsec" ,ghc-parsec)
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

(define-public git-annex
  (package
    (name "git-annex")
    (version "6.20180926")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "git-annex/git-annex-" version ".tar.gz"))
       (sha256
        (base32
         "1251rj8h63y30sfqk0zh670yhz14p256y59n3590pg015pf3575d"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags
       '("--flags=-Android -Assistant -Pairing -S3 -Webapp -WebDAV")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-shell
           (lambda _
             (substitute* "Utility/Shell.hs"
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'configure 'factor-setup
           (lambda _
             ;; Factor out necessary build logic from the provided
             ;; `Setup.hs' script.  The script as-is does not work because
             ;; it cannot find its dependencies, and there is no obvious way
             ;; to tell it where to look.  Note that we do not preserve the
             ;; code that installs man pages here.
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
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-annex-shell"))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-remote-tor-annex"))
               #t))))))
    (inputs
     `(("curl" ,curl)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-async" ,ghc-async)
       ("ghc-bloomfilter" ,ghc-bloomfilter)
       ("ghc-byteable" ,ghc-byteable)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-crypto-api" ,ghc-crypto-api)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-disk-free-space" ,ghc-disk-free-space)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-edit-distance" ,ghc-edit-distance)
       ("ghc-esqueleto" ,ghc-esqueleto)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-feed" ,ghc-feed)
       ("ghc-free" ,ghc-free)
       ("ghc-hslogger" ,ghc-hslogger)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-conduit" ,ghc-http-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-ifelse" ,ghc-ifelse)
       ("ghc-memory" ,ghc-memory)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-network" ,ghc-network)
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
       ("ghc-stm" ,ghc-stm)
       ("ghc-stm-chans" ,ghc-stm-chans)
       ("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-text" ,ghc-text)
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
       ("ghc-tasty-rerun" ,ghc-tasty-rerun)))
    (home-page "https://git-annex.branchable.com/")
    (synopsis "Manage files with Git, without checking in their contents")
    (description "This package allows managing files with Git, without
checking the file contents into Git.  It can store files in many places,
such as local hard drives and cloud storage services.  It can also be
used to keep a folder in sync between computers.")
    ;; The web app is released under the AGPLv3+.
    (license (list license:gpl3+
                   license:agpl3+))))
