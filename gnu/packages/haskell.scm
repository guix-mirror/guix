;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
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

(define-module (gnu packages haskell)
  #:use-module (ice-9 regex)
  #:use-module ((guix licenses) #:select (bsd-3 lgpl2.1 expat))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages xorg))

(define ghc-bootstrap-x86_64-7.8.4
  (origin
    (method url-fetch)
    (uri
     "https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz")
    (sha256
     (base32
      "13azsl53xgj20mi1hj9x0xb32vvcvs6cpmvwx6znxhas7blh0bpn"))))

(define ghc-bootstrap-i686-7.8.4
  (origin
    (method url-fetch)
    (uri
     "https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-i386-unknown-linux-deb7.tar.xz")
    (sha256
     (base32
      "0wj5s435j0zgww70bj1d3f6wvnnpzlxwvwcyh2qv4qjq5z8j64kg"))))

;; 43 tests out of 3965 fail.
;;
;; Most of them do not appear to be serious:
;;
;; - some tests generate files referring to "/bin/sh" and "/bin/ls". I've not
;;   figured out how these references are generated.
;;
;; - Some tests allocate more memory than expected (ca. 3% above upper limit)
;;
;; - Some tests try to load unavailable libriries: Control.Concurrent.STM,
;;   Data.Vector, Control.Monad.State.
;;
;; - Test posix010 tries to check the existence of a user on the system:
;;   getUserEntryForName: does not exist (no such user)
(define-public ghc
  (package
    (name "ghc")
    (version "7.10.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.haskell.org/ghc/dist/"
                          version "/" name "-" version "-src.tar.xz"))
      (sha256
       (base32
        "1x8m4rp2v7ydnrz6z9g8x7z3x3d3pxhv2pixy7i7hkbqbdsp7kal"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (outputs '("out" "doc"))
    (inputs
     `(("gmp" ,gmp)
       ("ncurses" ,ncurses)
       ("libffi" ,libffi)
       ("libedit" ,libedit)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (sha256
            (base32
             "0qp9da9ar87zbyn6wjgacd2ic1vgzbi3cklxnhsmjqyafv9qaj4b"))))))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)                ; for tests (fails with python-3)
       ("ghostscript" ,ghostscript)        ; for tests
       ("patchelf" ,patchelf)
       ;; GHC is built with GHC. Therefore we need bootstrap binaries.
       ("ghc-binary"
        ,(if (string-match "x86_64" (or (%current-target-system) (%current-system)))
             ghc-bootstrap-x86_64-7.8.4
             ghc-bootstrap-i686-7.8.4))))
    (arguments
     `(#:test-target "test"
       ;; We get a smaller number of test failures by disabling parallel test
       ;; execution.
       #:parallel-tests? #f

       ;; The DSOs use $ORIGIN to refer to each other, but (guix build
       ;; gremlin) doesn't support it yet, so skip this phase.
       #:validate-runpath? #f

       ;; Don't pass --build=<triplet>, because the configure script
       ;; auto-detects slightly different triplets for --host and --target and
       ;; then complains that they don't match.
       #:build #f

       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build rpath)
                  (srfi srfi-26)
                  (srfi srfi-1))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build rpath))
       #:configure-flags
       (list
        (string-append "--with-gmp-libraries="
                       (assoc-ref %build-inputs "gmp") "/lib")
        (string-append "--with-gmp-includes="
                       (assoc-ref %build-inputs "gmp") "/include")
        "--with-system-libffi"
        (string-append "--with-ffi-libraries="
                       (assoc-ref %build-inputs "libffi") "/lib")
        (string-append "--with-ffi-includes="
                       (assoc-ref %build-inputs "libffi") "/include"))
       ;; FIXME: The user-guide needs dblatex, docbook-xsl and docbook-utils.
       ;; Currently we do not have the last one.
       ;; #:make-flags
       ;; (list "BUILD_DOCBOOK_HTML = YES")
       #:phases
       (let* ((ghc-bootstrap-path
               (string-append (getcwd) "/" ,name "-" ,version "/ghc-bin"))
              (ghc-bootstrap-prefix
               (string-append ghc-bootstrap-path "/usr" )))
         (alist-cons-after
          'unpack-bin 'unpack-testsuite-and-fix-bins
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (with-directory-excursion ".."
              (copy-file (assoc-ref inputs "ghc-testsuite")
                         "ghc-testsuite.tar.xz")
              (system* "tar" "xvf" "ghc-testsuite.tar.xz"))
            (substitute*
                (list "testsuite/timeout/Makefile"
                      "testsuite/timeout/timeout.py"
                      "testsuite/timeout/timeout.hs"
                      "testsuite/tests/rename/prog006/Setup.lhs"
                      "testsuite/tests/programs/life_space_leak/life.test"
                      "libraries/process/System/Process/Internals.hs"
                      "libraries/unix/cbits/execvpe.c")
              (("/bin/sh") (which "sh"))
              (("/bin/rm") "rm"))
            #t)
          (alist-cons-after
           'unpack 'unpack-bin
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p ghc-bootstrap-prefix)
             (with-directory-excursion ghc-bootstrap-path
               (copy-file (assoc-ref inputs "ghc-binary")
                          "ghc-bin.tar.xz")
               (zero? (system* "tar" "xvf" "ghc-bin.tar.xz"))))
           (alist-cons-before
            'install-bin 'configure-bin
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((binaries
                      (list
                       "./utils/ghc-pwd/dist-install/build/tmp/ghc-pwd"
                       "./utils/hpc/dist-install/build/tmp/hpc"
                       "./utils/haddock/dist/build/tmp/haddock"
                       "./utils/hsc2hs/dist-install/build/tmp/hsc2hs"
                       "./utils/runghc/dist-install/build/tmp/runghc"
                       "./utils/ghc-cabal/dist-install/build/tmp/ghc-cabal"
                       "./utils/hp2ps/dist/build/tmp/hp2ps"
                       "./utils/ghc-pkg/dist-install/build/tmp/ghc-pkg"
                       "./utils/unlit/dist/build/tmp/unlit"
                       "./ghc/stage2/build/tmp/ghc-stage2"))
                     (gmp (assoc-ref inputs "gmp"))
                     (gmp-lib (string-append gmp "/lib"))
                     (gmp-include (string-append gmp "/include"))
                     (ncurses-lib
                      (string-append (assoc-ref inputs "ncurses") "/lib"))
                     (ld-so (string-append (assoc-ref inputs "libc")
                                           ,(glibc-dynamic-linker)))
                     (libtinfo-dir
                      (string-append ghc-bootstrap-prefix
                                     "/lib/ghc-7.8.4/terminfo-0.4.0.0")))
                (with-directory-excursion
                    (string-append ghc-bootstrap-path "/ghc-7.8.4")
                  (setenv "CONFIG_SHELL" (which "bash"))
                  (setenv "LD_LIBRARY_PATH" gmp-lib)
                  ;; The binaries have "/lib64/ld-linux-x86-64.so.2" hardcoded.
                  (for-each
                   (cut system* "patchelf" "--set-interpreter" ld-so <>)
                   binaries)
                  ;; The binaries include a reference to libtinfo.so.5 which
                  ;; is a subset of libncurses.so.5.  We create a symlink in a
                  ;; directory included in the bootstrap binaries rpath.
                  (mkdir-p libtinfo-dir)
                  (symlink
                   (string-append ncurses-lib "/libncursesw.so."
                                  ,(version-major+minor
                                    (package-version ncurses)))
                   (string-append libtinfo-dir "/libtinfo.so.5"))
                  (setenv "PATH"
                          (string-append (getenv "PATH") ":"
                                         ghc-bootstrap-prefix "/bin"))
                  (system*
                   (string-append (getcwd) "/configure")
                   (string-append "--prefix=" ghc-bootstrap-prefix)
                   (string-append "--with-gmp-libraries=" gmp-lib)
                   (string-append "--with-gmp-includes=" gmp-include)))))
            (alist-cons-before
             'configure 'install-bin
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (with-directory-excursion
                   (string-append ghc-bootstrap-path "/ghc-7.8.4")
                 (zero? (system* "make" "install"))))
             %standard-phases)))))))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license bsd-3)))

(define-public ghc-data-default
  (package
    (name "ghc-data-default")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/data-default/data-default-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0d1hm0l9kim3kszshr4msmgzizrzha48gz2kb7b61p7n3gs70m7c"))))
    (build-system haskell-build-system)
    (propagated-inputs
     `(("ghc-data-default-class"
        ,ghc-data-default-class)
       ("ghc-data-default-instances-base"
        ,ghc-data-default-instances-base)
       ("ghc-data-default-instances-containers"
        ,ghc-data-default-instances-containers)
       ("ghc-data-default-instances-dlist"
        ,ghc-data-default-instances-dlist)
       ("ghc-data-default-instances-old-locale"
        ,ghc-data-default-instances-old-locale)))
    (home-page "http://hackage.haskell.org/package/data-default")
    (synopsis "Types with default values")
    (description
     "This package defines a class for types with a default value, and
provides instances for types from the base, containers, dlist and old-locale
packages.")
    (license bsd-3)))

(define-public ghc-data-default-class
  (package
    (name "ghc-data-default-class")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/data-default-class/"
             "data-default-class-" version ".tar.gz"))
       (sha256
        (base32 "0ccgr3jllinchqhw3lsn73ic6axk4196if5274rr1rghls0fxj5d"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/data-default-class")
    (synopsis "Types with default values")
    (description
     "This package defines a class for types with default values.")
    (license bsd-3)))

(define-public ghc-data-default-instances-base
  (package
    (name "ghc-data-default-instances-base")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/"
             "data-default-instances-base/"
             "data-default-instances-base-" version ".tar.gz"))
       (sha256
        (base32 "1832nq6by91f1iw73ycvkbgn8kpra83pvf2q61hy47xffh0zy4pb"))))
    (build-system haskell-build-system)
    (propagated-inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)))
    (home-page "http://hackage.haskell.org/package/data-default-instances-base")
    (synopsis "Default instances for types in base")
    (description
     "This package provides default instances for types from the base
package.")
    (license bsd-3)))

(define-public ghc-data-default-instances-containers
  (package
    (name "ghc-data-default-instances-containers")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/"
             "data-default-instances-containers/"
             "data-default-instances-containers-" version ".tar.gz"))
       (sha256
        (base32 "06h8xka031w752a7cjlzghvr8adqbl95xj9z5zc1b62w02phfpm5"))))
    (build-system haskell-build-system)
    (propagated-inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)))
    (home-page "http://hackage.haskell.org/package/data-default-instances-containers")
    (synopsis "Default instances for types in containers")
    (description "Provides default instances for types from the containers
package.")
    (license bsd-3)))

(define-public ghc-data-default-instances-dlist
  (package
    (name "ghc-data-default-instances-dlist")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/"
             "data-default-instances-dlist/"
             "data-default-instances-dlist-" version ".tar.gz"))
       (sha256
        (base32 "0narkdqiprhgayjiawrr4390h4rq4pl2pb6mvixbv2phrc8kfs3x"))))
    (build-system haskell-build-system)
    (propagated-inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-dlist" ,ghc-dlist)))
    (home-page "http://hackage.haskell.org/package/data-default-instances-dlist")
    (synopsis "Default instances for types in dlist")
    (description "Provides default instances for types from the dlist
package.")
    (license bsd-3)))

(define-public ghc-old-locale
  (package
    (name "ghc-old-locale")
    (version "1.0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/old-locale/old-locale-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0l3viphiszvz5wqzg7a45zp40grwlab941q5ay29iyw8p3v8pbyv"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/old-locale")
    (synopsis "Adapt to locale conventions")
    (description
     "This package provides the ability to adapt to locale conventions such as
date and time formats.")
    (license bsd-3)))

(define-public ghc-old-time
  (package
    (name "ghc-old-time")
    (version "1.1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/old-time/old-time-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1h9b26s3kfh2k0ih4383w90ibji6n0iwamxp6rfp2lbq1y5ibjqw"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-/bin/sh
                    (lambda _
                      (setenv "CONFIG_SHELL" "sh"))))))
    (propagated-inputs
     `(("ghc-old-locale" ,ghc-old-locale)))
    (home-page "http://hackage.haskell.org/package/old-time")
    (synopsis "Time compatibility library for Haskell")
    (description "Old-time is a package for backwards compatibility with the
old @code{time} library.  For new projects, the newer
@uref{http://hackage.haskell.org/package/time, time library} is recommended.")
    (license bsd-3)))

(define-public ghc-data-default-instances-old-locale
  (package
    (name "ghc-data-default-instances-old-locale")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "http://hackage.haskell.org/package/"
              "data-default-instances-old-locale/"
              "data-default-instances-old-locale-" version ".tar.gz"))
        (sha256
          (base32 "00h81i5phib741yj517p8mbnc48myvfj8axzsw44k34m48lv1lv0"))))
    (build-system haskell-build-system)
    (propagated-inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-old-locale" ,ghc-old-locale)))
    (home-page
      "http://hackage.haskell.org/package/data-default-instances-old-locale")
    (synopsis "Default instances for types in old-locale")
    (description "Provides Default instances for types from the old-locale
  package.")
    (license bsd-3)))

(define-public ghc-dlist
  (package
    (name "ghc-dlist")
    (version "0.7.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/dlist/dlist-"
             version
             ".tar.gz"))
       (sha256
        (base32 "10rp96rryij7d8gz5kv8ygc6chm1624ck5mbnqs2a3fkdzqj2b9k"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/spl/dlist")
    (synopsis "Difference lists")
    (description
     "Difference lists are a list-like type supporting O(1) append.  This is
particularly useful for efficient logging and pretty printing (e.g. with the
Writer monad), where list append quickly becomes too expensive.")
    (license bsd-3)))

(define-public ghc-extensible-exceptions
  (package
    (name "ghc-extensible-exceptions")
    (version "0.1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://hackage.haskell.org/package/"
                           "extensible-exceptions/extensible-exceptions-"
                           version ".tar.gz"))
       (sha256
        (base32 "1273nqws9ij1rp1bsq5jc7k2jxpqa0svawdbim05lf302y0firbc"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/extensible-exceptions")
    (synopsis "Extensible exceptions for Haskell")
    (description
     "This package provides extensible exceptions for both new and old
versions of GHC (i.e., < 6.10).")
    (license bsd-3)))

(define-public cabal-install
 (package
  (name "cabal-install")
   (version "1.22.6.0")
   (source
    (origin
     (method url-fetch)
      (uri (string-append
            "http://hackage.haskell.org/package/cabal-install/cabal-install-"
            version
            ".tar.gz"))
      (sha256
       (base32 "1d5h7h2wjwc2s3dvsvzjgmmfrfl2312ym2h6kyjgm9wnaqw9w8wx"))))
   (arguments `(#:tests? #f)) ; FIXME: testing libraries are missing.
   (build-system haskell-build-system)
   (propagated-inputs
    `(("ghc-http" ,ghc-http)
      ("ghc-mtl" ,ghc-mtl)
      ("ghc-network-uri" ,ghc-network-uri)
      ("ghc-network" ,ghc-network)
      ("ghc-random" ,ghc-random)
      ("ghc-stm" ,ghc-stm)
      ("ghc-zlib" ,ghc-zlib)))
   (home-page "http://www.haskell.org/cabal/")
   (synopsis "Command-line interface for Cabal and Hackage")
   (description
    "The cabal command-line program simplifies the process of managing
Haskell software by automating the fetching, configuration, compilation and
installation of Haskell libraries and programs.")
   (license bsd-3)))

(define-public ghc-mtl
  (package
    (name "ghc-mtl")
    (version "2.2.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/mtl/mtl-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa"))))
    (build-system haskell-build-system)
    (home-page "http://github.com/ekmett/mtl")
    (synopsis
     "Monad classes, using functional dependencies")
    (description "Monad classes using functional dependencies, with instances
for various monad transformers, inspired by the paper 'Functional Programming
with Overloading and Higher-Order Polymorphism', by Mark P Jones, in 'Advanced
School of Functional Programming', 1995.  See
@uref{http://web.cecs.pdx.edu/~mpj/pubs/springschool.html, the paper}.")
    (license bsd-3)))

(define-public ghc-paths
  (package
    (name "ghc-paths")
    (version "0.1.0.9")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/ghc-paths/ghc-paths-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ibrr1dxa35xx20cpp8jzgfak1rdmy344dfwq4vlq013c6w8z9mg"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/simonmar/ghc-paths")
    (synopsis
     "Knowledge of GHC's installation directories")
    (description
     "Knowledge of GHC's installation directories.")
    (license bsd-3)))

(define-public ghc-utf8-string
  (package
    (name "ghc-utf8-string")
    (version "1.0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/utf8-string/utf8-string-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0h7imvxkahiy8pzr8cpsimifdfvv18lizrb33k6mnq70rcx9w2zv"))))
    (build-system haskell-build-system)
    (home-page "http://github.com/glguy/utf8-string/")
    (synopsis "Support for reading and writing UTF8 Strings")
    (description
     "A UTF8 layer for Strings.  The utf8-string package provides operations
for encoding UTF8 strings to Word8 lists and back, and for reading and writing
UTF8 without truncation.")
    (license bsd-3)))

(define-public ghc-setenv
  (package
    (name "ghc-setenv")
    (version "0.1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/setenv/setenv-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0cnbgrvb9byyahb37zlqrj05rj25v190crgcw8wmlgf0mwwxyn73"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/setenv")
    (synopsis "Library for setting environment variables")
    (description "This package provides a Haskell library for setting
environment variables.")
    (license expat)))

(define-public ghc-x11
  (package
    (name "ghc-x11")
    (version "1.6.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://hackage.haskell.org/package/X11/"
                           "X11-" version ".tar.gz"))
       (sha256
        (base32 "1kzjcynm3rr83ihqx2y2d852jc49da4p18gv6jzm7g87z22x85jj"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-sh
                              (lambda _
                                (setenv "CONFIG_SHELL" "sh")
                                #t)))))
    (build-system haskell-build-system)
    (inputs
     `(("libx11" ,libx11)
       ("libxrandr" ,libxrandr)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)))
    (propagated-inputs
     `(("ghc-data-default" ,ghc-data-default)))
    (home-page "https://github.com/haskell-pkg-janitors/X11")
    (synopsis "Bindings to the X11 graphics library")
    (description
     "This package provides Haskell bindings to the X11 graphics library.  The
bindings are a direct translation of the C bindings.")
    (license bsd-3)))

(define-public ghc-x11-xft
  (package
    (name "ghc-x11-xft")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://hackage.haskell.org/package/X11-xft/"
                           "X11-xft-" version ".tar.gz"))
       (sha256
        (base32 "1lgqb0s2qfwwgbvwxhjbi23rbwamzdi0l0slfr20c3jpcbp3zfjf"))))
    (propagated-inputs
     `(("ghc-x11" ,ghc-x11)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (inputs
     `(("libx11" ,libx11)
       ("libxft" ,libxft)
       ("xproto" ,xproto)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/X11-xft")
    (synopsis "Bindings to Xft")
    (description
     "Bindings to the Xft, X Free Type interface library, and some Xrender
parts.")
    (license lgpl2.1)))

(define-public ghc-stringbuilder
  (package
    (name "ghc-stringbuilder")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/stringbuilder/stringbuilder-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1ap95xphqnrhv64c2a137wqslkdmb2jjd9ldb17gs1pw48k8hrl9"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: circular dependencies with tests
                               ; enabled
    (home-page "http://hackage.haskell.org/package/stringbuilder")
    (synopsis "Writer monad for multi-line string literals")
    (description "This package provides a writer monad for multi-line string
literals.")
    (license expat)))

(define-public ghc-zlib
  (package
    (name "ghc-zlib")
    (version "0.5.4.2")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/zlib/zlib-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "15hhsk7z3gvm7sz2ic2z1ca5c6rpsln2rr391mdbm1bxlzc1gmkm"))))
    (build-system haskell-build-system)
    (inputs `(("zlib" ,zlib)))
    (home-page "http://hackage.haskell.org/package/zlib")
    (synopsis
     "Compression and decompression in the gzip and zlib formats")
    (description
     "This package provides a pure interface for compressing and decompressing
streams of data represented as lazy 'ByteString's.  It uses the zlib C library
so it has high performance.  It supports the 'zlib', 'gzip' and 'raw'
compression formats.  It provides a convenient high level API suitable for
most tasks and for the few cases where more control is needed it provides
access to the full zlib feature set.")
    (license bsd-3)))

(define-public ghc-stm
  (package
    (name "ghc-stm")
    (version "2.4.4")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/stm/stm-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0gc8zvdijp3rwmidkpxv76b4i0dc8dw6nbd92rxl4vxl0655iysx"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/stm")
    (synopsis "Software Transactional Memory")
    (description
     "A modular composable concurrency abstraction.")
    (license bsd-3)))

(define-public ghc-parallel
  (package
    (name "ghc-parallel")
    (version "3.2.0.6")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/parallel/parallel-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0hp6vf4zxsw6vz6lj505xihmnfhgjp39c9q7nyzlgcmps3xx6a5r"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/parallel")
    (synopsis "Parallel programming library")
    (description
     "This package provides a library for parallel programming.")
    (license bsd-3)))

(define-public ghc-text
  (package
    (name "ghc-text")
    (version "1.2.1.3")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/text/text-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0gzqx5cpkdhshbz9xss51mpyq23pnf8dwjz4h3irbv2ryaa4qdlq"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f)) ; FIXME: currently missing libraries used for tests.
    (home-page "https://github.com/bos/text")
    (synopsis "Efficient packed Unicode text type library")
    (description
     "An efficient packed, immutable Unicode text type (both strict and
lazy), with a powerful loop fusion optimization framework.

The 'Text' type represents Unicode character strings, in a time and
space-efficient manner.  This package provides text processing
capabilities that are optimized for performance critical use, both
in terms of large data quantities and high speed.")
    (license bsd-3)))

(define-public ghc-hashable
  (package
    (name "ghc-hashable")
    (version "1.2.3.3")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/hashable/hashable-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0kp4aj0x1iicz9qirpqxxqd8x5g1njbapxk1d90n406w3xykz4pw"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f)) ; FIXME: currently missing libraries used for tests.
    ;; these inputs are necessary to use this library
    (propagated-inputs
     `(("ghc-text" ,ghc-text)))
    (home-page "http://github.com/tibbe/hashable")
    (synopsis
     "Class for types that can be converted to a hash value")
    (description
     "This package defines a class, 'Hashable', for types that can be
converted to a hash value.  This class exists for the benefit of hashing-based
data structures.  The package provides instances for basic types and a way to
combine hash values.")
    (license bsd-3)))

(define-public ghc-hunit
  (package
    (name "ghc-hunit")
    (version "1.2.5.2")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/HUnit/HUnit-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0hcs6qh8bqhip1kkjjnw7ccgcsmawdz5yvffjj5y8zd2vcsavx8a"))))
    (build-system haskell-build-system)
    (home-page "http://hunit.sourceforge.net/")
    (synopsis "Unit testing framework for Haskell")
    (description
     "HUnit is a unit testing framework for Haskell, inspired by the
JUnit tool for Java.")
    (license bsd-3)))

(define-public ghc-random
  (package
    (name "ghc-random")
    (version "1.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/random/random-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0nis3lbkp8vfx8pkr6v7b7kr5m334bzb0fk9vxqklnp2aw8a865p"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/random")
    (synopsis "Random number library")
    (description "This package provides a basic random number generation
library, including the ability to split random number generators.")
    (license bsd-3)))

(define-public ghc-primitive
  (package
    (name "ghc-primitive")
    (version "0.6.1.0")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/primitive/primitive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1j1q7l21rdm8kfs93vibr3xwkkhqis181w2k6klfhx5g5skiywwk"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/haskell/primitive")
    (synopsis "Primitive memory-related operations")
    (description
     "This package provides various primitive memory-related operations.")
    (license bsd-3)))

(define-public ghc-tf-random
  (package
    (name "ghc-tf-random")
    (version "0.5")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/tf-random/tf-random-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0445r2nns6009fmq0xbfpyv7jpzwv0snccjdg7hwj4xk4z0cwc1f"))))
    (build-system haskell-build-system)
    ;; these inputs are necessary to use this package
    (propagated-inputs
     `(("ghc-primitive" ,ghc-primitive)
       ("ghc-random" ,ghc-random)))
    (home-page "http://hackage.haskell.org/package/tf-random")
    (synopsis "High-quality splittable pseudorandom number generator")
    (description "This package contains an implementation of a high-quality
splittable pseudorandom number generator.  The generator is based on a
cryptographic hash function built on top of the ThreeFish block cipher.  See
the paper \"Splittable Pseudorandom Number Generators Using Cryptographic
Hashing\" by Claessen, Pałka for details and the rationale of the design.")
    (license bsd-3)))

(define-public ghc-transformers-compat
  (package
    (name "ghc-transformers-compat")
    (version "0.4.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/transformers-compat"
             "/transformers-compat-" version ".tar.gz"))
       (sha256
        (base32
         "0lmg8ry6bgigb0v2lg0n74lxi8z5m85qq0qi4h1k9llyjb4in8ym"))))
    (build-system haskell-build-system)
    (home-page "http://github.com/ekmett/transformers-compat/")
    (synopsis "Small compatibility shim between transformers 0.3 and 0.4")
    (description "This package includes backported versions of types that were
added to transformers in transformers 0.3 and 0.4 for users who need strict
transformers 0.2 or 0.3 compatibility to run on old versions of the platform,
but also need those types.")
    (license bsd-3)))

(define-public ghc-regex-base
  (package
    (name "ghc-regex-base")
    (version "0.93.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/regex-base/regex-base-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0y1j4h2pg12c853nzmczs263di7xkkmlnsq5dlp5wgbgl49mgp10"))))
    (build-system haskell-build-system)
    (propagated-inputs
     `(("ghc-mtl" ,ghc-mtl)))
    (home-page
     "http://sourceforge.net/projects/lazy-regex")
    (synopsis "Replaces/Enhances Text.Regex")
    (description "@code{Text.Regex.Base} provides the interface API for
regex-posix, regex-pcre, regex-parsec, regex-tdfa, regex-dfa.")
    (license bsd-3)))

(define-public ghc-appar
  (package
    (name "ghc-appar")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/appar/appar-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "09jb9ij78fdkz2qk66rw99q19qnm504dpv0yq0pjsl6xwjmndsjq"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/appar")
    (synopsis "Simple applicative parser")
    (description "This package provides a simple applicative parser in Parsec
style.")
    (license bsd-3)))

(define-public ghc-safe
  (package
    (name "ghc-safe")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/safe/safe-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1jdnp5zhvalf1xy8i872n29nljfjz6lnl9ghj80ffisrnnkrwcfh"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ndmitchell/safe#readme")
    (synopsis "Library of safe (exception free) functions")
    (description "This library provides wrappers around @code{Prelude} and
@code{Data.List} functions, such as @code{head} and @code{!!}, that can throw
exceptions.")
    (license bsd-3)))

(define-public ghc-generic-deriving
  (package
    (name "ghc-generic-deriving")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/generic-deriving/generic-deriving-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1kc6lhdanls6kgpk8xv5xi14lz1sngcd8xn930hkf7ilq4kxkcr6"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/generic-deriving")
    (synopsis "Generalise the deriving mechanism to arbitrary classes")
    (description "This package provides functionality for generalising the
deriving mechanism in Haskell to arbitrary classes.")
    (license bsd-3)))

(define-public ghc-pcre-light
  (package
    (name "ghc-pcre-light")
    (version "0.4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/pcre-light/pcre-light-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0l1df2sk5qwf424bvb8mbdkr2xjg43fi92n5r22yd7vm1zz0jqvf"))))
    (build-system haskell-build-system)
    (inputs
     `(("pcre" ,pcre)))
    (home-page "https://github.com/Daniel-Diaz/pcre-light")
    (synopsis "Haskell library for Perl 5 compatible regular expressions")
    (description "This package provides a small, efficient, and portable regex
library for Perl 5 compatible regular expressions.  The PCRE library is a set
of functions that implement regular expression pattern matching using the same
syntax and semantics as Perl 5.")
    (license bsd-3)))

(define-public ghc-logict
  (package
    (name "ghc-logict")
    (version "0.6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/logict/logict-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "07hnirv6snnym2r7iijlfz00b60jpy2856zvqxh989q0in7bd0hi"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-mtl" ,ghc-mtl)))
    (home-page "http://code.haskell.org/~dolio/")
    (synopsis "Backtracking logic-programming monad")
    (description "This library provides a continuation-based, backtracking,
logic programming monad.  An adaptation of the two-continuation implementation
found in the paper \"Backtracking, Interleaving, and Terminating Monad
Transformers\" available @uref{http://okmij.org/ftp/papers/LogicT.pdf,
online}.")
    (license bsd-3)))

(define-public ghc-xml
  (package
    (name "ghc-xml")
    (version "1.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/xml/xml-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0g814lj7vaxvib2g3r734221k80k7ap9czv9hinifn8syals3l9j"))))
    (build-system haskell-build-system)
    (propagated-inputs
     `(("ghc-text" ,ghc-text)))
    (home-page "http://code.galois.com")
    (synopsis "Simple XML library for Haskell")
    (description "This package provides a simple XML library for Haskell.")
    (license bsd-3)))

(define-public ghc-exceptions
  (package
    (name "ghc-exceptions")
    (version "0.8.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/exceptions/exceptions-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1x1bk1jf42k1gigiqqmkkh38z2ffhx8rsqiszdq3f94m2h6kw2h7"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: Missing test-framework package.
    (propagated-inputs
     `(("ghc-stm" ,ghc-stm)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "http://github.com/ekmett/exceptions/")
    (synopsis "Extensible optionally-pure exceptions")
    (description "This library provides extensible optionally-pure exceptions
for Haskell.")
    (license bsd-3)))

(define-public ghc-temporary
  (package
    (name "ghc-temporary")
    (version "1.2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/temporary/temporary-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0is67bmsjmbbw6wymhis8wyq9gax3sszm573p5719fx2c9z9r24a"))))
    (build-system haskell-build-system)
    (propagated-inputs `(("ghc-exceptions" ,ghc-exceptions)))
    (home-page "http://www.github.com/batterseapower/temporary")
    (synopsis "Temporary file and directory support")
    (description "The functions for creating temporary files and directories
in the Haskelll base library are quite limited.  This library just repackages
the Cabal implementations of its own temporary file and folder functions so
that you can use them without linking against Cabal or depending on it being
installed.")
    (license bsd-3)))

(define-public ghc-silently
  (package
    (name "ghc-silently")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/silently/silently-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0f9qm3f7y0hpxn6mddhhg51mm1r134qkvd2kr8r6192ka1ijbxnf"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ;; circular dependency with nanospec
    ;; (inputs
    ;;  `(("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/hspec/silently")
    (synopsis "Prevent writing to stdout")
    (description "This package provides functions to prevent or capture
writing to stdout and other handles.")
    (license bsd-3)))

(define-public ghc-quickcheck-unicode
  (package
    (name "ghc-quickcheck-unicode")
    (version "1.0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/quickcheck-unicode/quickcheck-unicode-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1a8nl6x7l9b22yx61wm0bh2n1xzb1hd5i5zgg1w4fpaivjnrrhi4"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page
     "https://github.com/bos/quickcheck-unicode")
    (synopsis "Generator functions Unicode-related tests")
    (description "This package provides generator and shrink functions for
testing Unicode-related software.")
    (license bsd-3)))

(define-public ghc-quickcheck-io
  (package
    (name "ghc-quickcheck-io")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/quickcheck-io/quickcheck-io-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1kf1kfw9fsmly0rvzvdf6jvdw10qhkmikyj0wcwciw6wad95w9sh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page
     "https://github.com/hspec/quickcheck-io#readme")
    (synopsis "Use HUnit assertions as QuickCheck properties")
    (description "This package provides an orphan instance that allows you to
use HUnit assertions as QuickCheck properties.")
    (license expat)))

(define-public ghc-quickcheck
  (package
    (name "ghc-quickcheck")
    (version "2.8.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/QuickCheck/QuickCheck-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0fvnfl30fxmj5q920l13641ar896d53z0z6z66m7c1366lvalwvh"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f  ; FIXME: currently missing libraries used for tests.
       #:configure-flags '("-f base4")))
    ;; these inputs are necessary to use this package
    (propagated-inputs
     `(("ghc-tf-random" ,ghc-tf-random)))
    (home-page
     "https://github.com/nick8325/quickcheck")
    (synopsis
     "Automatic testing of Haskell programs")
    (description
     "QuickCheck is a library for random testing of program properties.")
    (license bsd-3)))

(define-public ghc-case-insensitive
  (package
    (name "ghc-case-insensitive")
    (version "1.2.0.4")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/case-insensitive/case-insensitive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "07nm40r9yw2p9qsfp3pjbsmyn4dabrxw34p48171zmccdd5hv0v3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)))
    ;; these inputs are necessary to use this library
    (propagated-inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-hashable" ,ghc-hashable)))
    (arguments
     `(#:tests? #f)) ; FIXME: currently missing libraries used for tests.
    (home-page
     "https://github.com/basvandijk/case-insensitive")
    (synopsis "Case insensitive string comparison")
    (description
     "The module 'Data.CaseInsensitive' provides the 'CI' type constructor
which can be parameterised by a string-like type like: 'String', 'ByteString',
'Text', etc..  Comparisons of values of the resulting type will be insensitive
to cases.")
    (license bsd-3)))

(define-public ghc-syb
  (package
    (name "ghc-syb")
    (version "0.6")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/syb/syb-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1p3cnqjm13677r4a966zffzhi9b3a321aln8zs8ckqj0d9z1z3d3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-mtl" ,ghc-mtl)))
    (home-page
     "http://www.cs.uu.nl/wiki/GenericProgramming/SYB")
    (synopsis "Scrap Your Boilerplate")
    (description "This package contains the generics system described in the
/Scrap Your Boilerplate/ papers (see
@uref{http://www.cs.uu.nl/wiki/GenericProgramming/SYB, the website}).  It
defines the 'Data' class of types permitting folding and unfolding of
constructor applications, instances of this class for primitive types, and a
variety of traversals.")
    (license bsd-3)))

(define-public ghc-fgl
  (package
    (name "ghc-fgl")
    (version "5.5.1.0")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/fgl/fgl-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0rcmz0xlyr1wj490ffja29z1jgl51gz19ka609da6bx39bwx7nga"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-mtl" ,ghc-mtl)))
    (home-page "http://web.engr.oregonstate.edu/~erwig/fgl/haskell")
    (synopsis
     "Martin Erwig's Functional Graph Library")
    (description "The functional graph library, FGL, is a collection of type
and function definitions to address graph problems.  The basis of the library
is an inductive definition of graphs in the style of algebraic data types that
encourages inductive, recursive definitions of graph algorithms.")
    (license bsd-3)))

(define-public ghc-unordered-containers
  (package
    (name "ghc-unordered-containers")
    (version "0.2.5.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/unordered-containers"
             "/unordered-containers-" version ".tar.gz"))
       (sha256
        (base32
         "06l1xv7vhpxly75saxdrbc6p2zlgz1az278arfkz4rgawfnphn3f"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    ;; these inputs are necessary to use this library
    (propagated-inputs `(("ghc-hashable" ,ghc-hashable)))
    (arguments
     `(#:tests? #f)) ; FIXME: currently missing libraries used for tests.
    (home-page
     "https://github.com/tibbe/unordered-containers")
    (synopsis
     "Efficient hashing-based container types")
    (description
     "Efficient hashing-based container types.  The containers have been
optimized for performance critical use, both in terms of large data quantities
and high speed.")
    (license bsd-3)))

(define-public ghc-base64-bytestring
  (package
    (name "ghc-base64-bytestring")
    (version "1.0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/base64-bytestring/base64-bytestring-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0l1v4ddjdsgi9nqzyzcxxj76rwar3lzx8gmwf2r54bqan3san9db"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))         ; FIXME: testing libraries are missing.
    (home-page "https://github.com/bos/base64-bytestring")
    (synopsis "Base64 encoding and decoding for ByteStrings")
    (description "This library provides fast base64 encoding and decoding for
Haskell @code{ByteString}s.")
    (license bsd-3)))

(define-public ghc-annotated-wl-pprint
  (package
    (name "ghc-annotated-wl-pprint")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/annotated-wl-pprint"
             "/annotated-wl-pprint-" version
             ".tar.gz"))
       (sha256
        (base32
         "061xfz6qany3wf95csl8dcik2pz22cn8iv1qchhm16isw5zjs9hc"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/david-christiansen/annotated-wl-pprint")
    (synopsis
     "The Wadler/Leijen Pretty Printer, with annotation support")
    (description "This is a modified version of wl-pprint, which was based on
Wadler's paper \"A Prettier Printer\".  This version allows the library user
to annotate the text with semantic information, which can later be rendered in
a variety of ways.")
    (license bsd-3)))

(define-public ghc-ansi-wl-pprint
  (package
    (name "ghc-ansi-wl-pprint")
    (version "0.6.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/ansi-wl-pprint/ansi-wl-pprint-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "025pyphsjf0dnbrmj5nscbi6gzyigwgp3ifxb3psn7kji6mfr29p"))))
    (build-system haskell-build-system)
    (propagated-inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)))
    (home-page "http://github.com/ekmett/ansi-wl-pprint")
    (synopsis "Wadler/Leijen Pretty Printer for colored ANSI terminal output")
    (description "This is a pretty printing library based on Wadler's paper
\"A Prettier Printer\".  It has been enhanced with support for ANSI terminal
colored output using the ansi-terminal package.")
    (license bsd-3)))

(define-public ghc-split
  (package
    (name "ghc-split")
    (version "0.2.2")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/split/split-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0xa3j0gwr6k5vizxybnzk5fgb3pppgspi6mysnp2gwjp2dbrxkzr"))
       (modules '((guix build utils)))
       (snippet
        ;; The Cabal file on Hackage is updated, but the tar.gz does not
        ;; include it.  See
        ;; <https://hackage.haskell.org/package/split-0.2.2/revisions/>.
        '(substitute* "split.cabal"
           (("base <4.8") "base <4.9")))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "http://hackage.haskell.org/package/split")
    (synopsis "Combinator library for splitting lists")
    (description "This package provides a collection of Haskell functions for
splitting lists into parts, akin to the @code{split} function found in several
mainstream languages.")
    (license bsd-3)))

(define-public ghc-parsec
  (package
    (name "ghc-parsec")
    (version "3.1.9")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/parsec/parsec-"
             version
             ".tar.gz"))
       (sha256
        (base32 "1ja20cmj6v336jy87c6h3jzjp00sdbakwbdwp11iln499k913xvi"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)))
    ;; these inputs are necessary to use this library
    (propagated-inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-mtl" ,ghc-mtl)))
    (arguments
     `(#:tests? #f)) ; FIXME: currently missing libraries used for tests.
    (home-page
     "https://github.com/aslatter/parsec")
    (synopsis "Monadic parser combinators")
    (description "Parsec is a parser library.  It is simple, safe, well
documented, has extensive libraries, good error messages, and is fast.  It is
defined as a monad transformer that can be stacked on arbitrary monads, and it
is also parametric in the input stream type.")
    (license bsd-3)))

(define-public ghc-vector
  (package
    (name "ghc-vector")
    (version "0.11.0.0")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/vector/vector-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1r1jlksy7b0kb0fy00g64isk6nyd9wzzdq31gx5v1wn38knj0lqa"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    ;; these inputs are necessary to use this library
    (propagated-inputs
     `(("ghc-primitive" ,ghc-primitive)))
    (arguments
     `(#:tests? #f))      ; FIXME: currently missing libraries used for tests.
    (home-page "https://github.com/haskell/vector")
    (synopsis "Efficient Arrays")
    (description "This library provides an efficient implementation of
Int-indexed arrays (both mutable and immutable), with a powerful loop
optimisation framework.")
    (license bsd-3)))

(define-public ghc-network
  (package
    (name "ghc-network")
    (version "2.6.2.1")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/network/network-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1yhvpd4wigz165jvyvw9zslx7lgqdj63jh3zv5s74b5ykdfa3zd3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (arguments
     `(#:tests? #f  ; FIXME: currently missing libraries used for tests.
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-sh
                     (lambda _ (setenv "CONFIG_SHELL" "sh"))))))
    (home-page "https://github.com/haskell/network")
    (synopsis "Low-level networking interface")
    (description
     "This package provides a low-level networking interface.")
    (license bsd-3)))

(define-public ghc-network-uri
  (package
    (name "ghc-network-uri")
    (version "2.6.0.3")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/network-uri/network-uri-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1pwbqb2rk4rnvllvdch42p5368xcvpkanp7bxckdhxya8zzwvhhg"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-network" ,ghc-network)))
    (arguments
     `(#:tests? #f))  ; FIXME: currently missing libraries used for tests.
    (propagated-inputs
     `(("ghc-parsec" ,ghc-parsec)))
    (home-page
     "https://github.com/haskell/network-uri")
    (synopsis "Library for URI manipulation")
    (description "This package provides an URI manipulation interface.  In
'network-2.6' the 'Network.URI' module was split off from the 'network'
package into this package.")
    (license bsd-3)))

(define-public ghc-ansi-terminal
  (package
    (name "ghc-ansi-terminal")
    (version "0.6.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/ansi-terminal/ansi-terminal-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0hpfw0k025y681m9ml1c712skrb1p4vh7z5x1f0ci9ww7ssjrh2d"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/feuerbach/ansi-terminal")
    (synopsis "ANSI terminal support for Haskell")
    (description "This package provides ANSI terminal support for Haskell.  It
allows cursor movement, screen clearing, color output showing or hiding the
cursor, and changing the title.")
    (license bsd-3)))

(define-public ghc-http
  (package
    (name "ghc-http")
    (version "4000.2.20")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/HTTP/HTTP-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0nyqdxr5ls2dxkf4a1f3x15xzwdm46ppn99nkcbhswlr6s3cq1s4"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (propagated-inputs
     `(("ghc-old-time" ,ghc-old-time)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)))
    (arguments
     `(#:tests? #f))  ; FIXME: currently missing libraries used for tests.
    (home-page "https://github.com/haskell/HTTP")
    (synopsis "Library for client-side HTTP")
    (description
     "The HTTP package supports client-side web programming in Haskell.  It
lets you set up HTTP connections, transmitting requests and processing the
responses coming back.")
    (license bsd-3)))

(define-public ghc-hspec
  (package
    (name "ghc-hspec")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/hspec/hspec-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0zqisxznlbszivsgy3irvf566zhcr6ipqqj3x9i7pj5hy913jwqf"))))
    (build-system haskell-build-system)
    (propagated-inputs
     `(("ghc-hspec-core" ,ghc-hspec-core)
       ("hspec-discover" ,hspec-discover)
       ("ghc-hspec-expectations" ,ghc-hspec-expectations)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)))
    (inputs
     `(("ghc-stringbuilder" ,ghc-stringbuilder)
       ("ghc-hspec-meta" ,ghc-hspec-meta)))
    (home-page "http://hspec.github.io/")
    (synopsis "Testing Framework for Haskell")
    (description "This library provides the Hspec testing framework for
Haskell, inspired by the Ruby library RSpec.")
    (license expat)))

(define-public ghc-hspec-expectations
  (package
    (name "ghc-hspec-expectations")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/hspec-expectations/hspec-expectations-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1w56jiqfyl237sr207gh3b0l8sr9layy0mdsgd5wknzb49mif6ip"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/sol/hspec-expectations")
    (synopsis "Catchy combinators for HUnit")
    (description "This library provides catchy combinators for HUnit, see
@uref{https://github.com/sol/hspec-expectations#readme, the README}.")
    (license expat)))

(define-public hspec-discover
  (package
    (name "hspec-discover")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/hspec-discover/hspec-discover-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0w3awzbljf4hqhxrjrxqa1lfcclg92bhmq641gz2q80vycspapzx"))))
    (build-system haskell-build-system)
    (arguments `(#:haddock? #f)) ; Haddock phase fails because there are no
                                 ; documentation files.
    (inputs `(("ghc-hspec-meta" ,ghc-hspec-meta)))
    (home-page "http://hspec.github.io/")
    (synopsis "Automatically discover and run Hspec tests")
    (description "hspec-discover is a tool which automatically discovers and
runs Hspec tests.")
    (license expat)))

(define-public ghc-hspec-core
  (package
    (name "ghc-hspec-core")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/hspec-core/hspec-core-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1wgd55k652jaf81nkvciyqi67ycj7zamr4nd9z1cqf8nr9fc3sa4"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: testing libraries are missing.
    (propagated-inputs
     `(("ghc-setenv" ,ghc-setenv)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-async" ,ghc-async)
       ("ghc-quickcheck-io" ,ghc-quickcheck-io)))
    (inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec-expectations" ,ghc-hspec-expectations)
       ("ghc-silently" ,ghc-silently)))
    (home-page "http://hspec.github.io/")
    (synopsis "Testing framework for Haskell")
    (description "This library exposes internal types and functions that can
be used to extend Hspec's functionality.")
    (license expat)))

(define-public ghc-hspec-meta
  (package
    (name "ghc-hspec-meta")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/hspec-meta/hspec-meta-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1fmqmgrzp135cxhmxxbaswkk4bqbpgfml00cmcz0d39n11vzpa5z"))))
    (build-system haskell-build-system)
    (propagated-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-async" ,ghc-async)
       ("ghc-hspec-expectations" ,ghc-hspec-expectations)
       ("ghc-setenv" ,ghc-setenv)
       ("ghc-random" ,ghc-random)
       ("ghc-quickcheck-io" ,ghc-quickcheck-io)))
    (home-page "http://hspec.github.io/")
    (synopsis "Version of Hspec to test Hspec itself")
    (description "This library provides a stable version of Hspec which is
used to test the in-development version of Hspec.")
    (license expat)))

(define-public ghc-async
  (package
    (name "ghc-async")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://hackage.haskell.org/package/async/async-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0azx4qk65a9a2gvqsfmz3w89m6shzr2iz0i5lly2zvly4n2d6m6v"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: testing libraries are missing.
    (propagated-inputs
     `(("ghc-stm" ,ghc-stm)))
    (home-page "https://github.com/simonmar/async")
    (synopsis "Library to run IO operations asynchronously")
    (description "Async provides a library to run IO operations
asynchronously, and wait for their results. It is a higher-level interface
over threads in Haskell, in which @code{Async a} is a concurrent thread that
will eventually deliver a value of type @code{a}.")
    (license bsd-3)))

;;; haskell.scm ends here
