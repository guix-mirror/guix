;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2015, 2018 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2017 Frederick Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages dlang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils))

(define-public rdmd
  (package
    (name "rdmd")
    (version "2.077.1")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/dlang/tools/archive/v" version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "0c8w373rv6iz3xfid94w40ncv2lr2ncxi662qsr4lda4aghczmq7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check) ; There is no Makefile, so there's no 'make check'.
         (replace
          'build
          (lambda _
            (invoke "ldc2" "rdmd.d")))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "rdmd" bin)))))))
    (native-inputs
     `(("ldc" ,ldc)))
    (home-page "https://github.com/D-Programming-Language/tools/")
    (synopsis "Specialized equivalent to 'make' for the D language")
    (description
     "rdmd is a companion to the dmd compiler that simplifies the typical
edit-compile-link-run or edit-make-run cycle to a rapid edit-run cycle.  Like
make and other tools, rdmd uses the relative dates of the files involved to
minimize the amount of work necessary.  Unlike make, rdmd tracks dependencies
and freshness without requiring additional information from the user.")
    (license license:boost1.0)))

(define-public ldc-bootstrap
    (package
      (name "ldc")
      (version "0.17.4")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/ldc-developers/ldc/archive/v"
                      version ".tar.gz"))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "1kw0j378k6bh0k66dvx99bjq8ilp8bb24w3jrmibn8rhmqv0d5q8"))))
      (build-system cmake-build-system)
      (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux"))
      (properties
       ;; Some of the tests take a very long time on ARMv7.  See
       ;; <https://lists.gnu.org/archive/html/guix-devel/2018-02/msg00312.html>.
       `((max-silent-time . ,(* 3600 3))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (source target)
                               (with-directory-excursion target
                                 (invoke "tar" "xvf"
                                         (assoc-ref inputs source)
                                         "--strip-components=1")))))
                 (unpack "phobos-src" "runtime/phobos")
                 (unpack "druntime-src" "runtime/druntime")
                 (unpack "dmd-testsuite-src" "tests/d2/dmd-testsuite")
                 #t)))
           (add-after 'unpack-submodule-sources 'patch-dmd2
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "dmd2/root/port.c"
                 ((" ::isnan") " isnan")
                 ((" ::isinf") " isinf")
                 (("#undef isnan") "")
                 (("#undef isinf") ""))
               #t))
           (add-after 'unpack-submodule-sources 'patch-phobos
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "runtime/phobos/std/process.d"
                 (("/bin/sh") (which "sh"))
                 (("echo") (which "echo")))
               (substitute* "runtime/phobos/std/datetime.d"
                 (("/usr/share/zoneinfo/")
                  (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo"))
                 (("tzName == \"[+]VERSION\"")
                  "(tzName == \"+VERSION\" || std.algorithm.endsWith(tzName, \"/leapseconds\"))"))
               (substitute* "tests/d2/dmd-testsuite/Makefile"
                 (("/bin/bash") (which "bash")))
               ;; FIXME: this test cannot be linked.
               (delete-file "tests/d2/dmd-testsuite/runnable/cppa.d")
               ;; the following two tests fail on i686
               (for-each delete-file '("tests/ir/attributes.d" "tests/ir/align.d")))))))
      (inputs
       `(("libconfig" ,libconfig)
         ("libedit" ,libedit)
         ("tzdata" ,tzdata)
         ("zlib" ,zlib)))
      (native-inputs
       `(("llvm" ,llvm-3.8)
         ("clang" ,clang-3.8)
         ("python-lit" ,python-lit)
         ("python-wrapper" ,python-wrapper)
         ("unzip" ,unzip)
         ("phobos-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/phobos/archive/ldc-v"
                   version ".tar.gz"))
             (sha256
              (base32
               "16x36kp46mqiihxx7jvr1d3mv3b96yfmhinb9lzinh2m4clr85wz"))
             (patches (search-patches "ldc-bootstrap-disable-tests.patch"))))
         ("druntime-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/druntime/archive/ldc-v"
                   version ".tar.gz"))
             (sha256
              (base32
               "0iw2xxhcbsc5f1707dgdzhff528363l4faqdk513gaxs2dhfx8vx"))))
         ("dmd-testsuite-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/dmd-testsuite/archive/ldc-v"
                   version ".tar.gz"))
             (sha256
              (base32
               "0z6ch930wjkg2vlnqkbliwxxxifad6ydsdpwdxwnajkb2kaxsjx4"))))))
      (home-page "http://wiki.dlang.org/LDC")
      (synopsis "LLVM-based compiler for the D programming language")
      (description
       "LDC is an LLVM compiler for the D programming language.  It is based on
the latest DMD compiler that was written in C and is used for
bootstrapping more recent compilers written in D.")
      ;; Most of the code is released under BSD-3, except for code originally
      ;; written for GDC, which is released under GPLv2+, and the DMD frontend,
      ;; which is released under the "Boost Software License version 1.0".
      (license (list license:bsd-3
                     license:gpl2+
                     license:boost1.0))))

(define-public ldc
  ;; Phobos, druntime and dmd-testsuite library dependencies do
  ;; not always have a newer release than the compiler, hence we
  ;; retain this variable.
  (let ((older-version "1.10.0")) ;; retain this because sometimes the libs are older
    (package
      (inherit ldc-bootstrap)
      (name "ldc")
      (version "1.10.0")
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/ldc-developers/ldc/archive/v"
                      version ".tar.gz"))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "16b1h9kwfggjw6ykc6sfs26ak6vypylsx9wmvp5m6x3cvi6g70yi"))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (source target)
                               (with-directory-excursion target
                                 (invoke "tar" "xvf"
                                         (assoc-ref inputs source)
                                         "--strip-components=1")))))
                 (unpack "phobos-src" "runtime/phobos")
                 (unpack "druntime-src" "runtime/druntime")
                 (unpack "dmd-testsuite-src" "tests/d2/dmd-testsuite"))))
           (add-after 'unpack-submodule-sources 'patch-phobos
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("runtime/phobos/std/process.d"
                              "tests/linking/linker_switches.d")
                 (("/bin/sh") (which "sh"))
                 (("echo") (which "echo")))
               (substitute* "tests/d2/dmd-testsuite/Makefile"
                            (("/bin/bash") (which "bash")))
               ;; disable unittests in the following files. We are discussing with
               ;; upstream
               (substitute* '("runtime/phobos/std/net/curl.d"
                              "runtime/phobos/std/datetime/systime.d"
                              "runtime/phobos/std/datetime/timezone.d"
                              )
                 (("version(unittest)") "version(skipunittest)")
                 ((" unittest") " version(skipunittest) unittest"))
               ;; the following tests require a more recent LLVM
               (delete-file "tests/compilable/ctfe_math.d")
               (delete-file "tests/debuginfo/nested_gdb.d")
               (delete-file "tests/debuginfo/classtypes_gdb.d")
               ;; the following tests plugins we don't have.
               (delete-file "tests/plugins/addFuncEntryCall/testPlugin.d")
               ;; the following tests requires AVX instruction set in the CPU.
               (substitute* "tests/d2/dmd-testsuite/runnable/test_cdvecfill.d"
                (("^// DISABLED: ") "^// DISABLED: linux64 "))
               #t))
           (replace 'check
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; some tests call into gdb binary which needs SHELL and CC set
                      (setenv "SHELL" (which "sh"))
                      (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
                      (invoke "make" "test" "-j" (number->string (parallel-job-count))))))))
      (native-inputs
       `(("llvm" ,llvm-6)
         ("clang" ,clang-6)
         ("ldc" ,ldc-bootstrap)
         ("python-lit" ,python-lit)
         ("python-wrapper" ,python-wrapper)
         ("unzip" ,unzip)
         ("gdb" ,gdb)
         ("phobos-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/phobos/archive/ldc-v"
                   older-version ".tar.gz"))
             (sha256
              (base32
               "0cpmrww00xf1qx38bcc22rr05qw41p00p45yb5fbwnfaccfwdn0s"))
             ;; This patch deactivates some tests that depend on network access
             ;; to pass.  It also deactivates some tests that have some reliance
             ;; on timezone.
             ;;
             ;; For the network tests, there's an effort to get a version flag
             ;; added to deactivate these tests for distribution packagers
             ;; that is being pursued at
             ;; <https://forum.dlang.org/post/zmdbdgnzrxyvtpqafvyg@forum.dlang.org>.
             ;; It also deactivates a test that requires /root
             (patches (search-patches "ldc-disable-phobos-tests.patch"))))
         ("druntime-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/druntime/archive/ldc-v"
                   older-version ".tar.gz"))
             (sha256
              (base32
               "1akh2vdi98jih8642yjbvv2vavxzrmq24kz8i3kfidg5ndqyv222"))))
         ("dmd-testsuite-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/dmd-testsuite/archive/ldc-v"
                   older-version ".tar.gz"))
             (sha256
              (base32
               "0z5x07qrbkpksshaymp11ir6jlmg9wjicxn6zhp8cya6i1ha9p99")))))))))

(define-public dub
  (package
    (name "dub")
    (version "1.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dlang/dub/archive/"
                                  "v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jvr1mmq8j77wnsrsg7x2xv8yfljqd6x8gn6yy7dd6h6y3cf408q"))))
   (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; it would have tested itself by installing some packages (vibe etc)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'build
           (lambda _
             (invoke "./build.sh")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "bin/dub" bin)
               #t))))))
    (inputs
     `(("curl" ,curl)))
    (native-inputs
     `(("ldc" ,ldc)))
    (home-page "https://code.dlang.org/getting_started")
    (synopsis "Package and build manager for D projects")
    (description
     "DUB is a package and build manager for applications and
libraries written in the D programming language.  It can
automatically retrieve a project's dependencies and integrate
them in the build process.

The design emphasis is on maximum simplicity for simple projects,
while providing the opportunity to customize things when
needed.")
    (license license:expat)))
