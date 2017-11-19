;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2015 Pjotr Prins <pjotr.guix@thebird.nl>
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

(define-module (gnu packages ldc)
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
  #:use-module (gnu packages textutils))

(define-public rdmd
  (package
    (name "rdmd")
    (version "2.073.0")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/dlang/tools/archive/v" version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "01if3ivnb7g2myfhymp4d9346s4vmvcl82i1kxfs5iza45almh7v"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check) ; There is no Makefile, so there's no 'make check'.
         (replace
          'build
          (lambda _
            (zero? (system* "ldc2" "rdmd.d"))))
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
  (let ((runtime-version "0.17.3"))
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
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (source target)
                               (with-directory-excursion target
                                 (zero? (system* "tar" "xvf"
                                                 (assoc-ref inputs source)
                                                 "--strip-components=1"))))))
                 (and (unpack "phobos-src" "runtime/phobos")
                      (unpack "druntime-src" "runtime/druntime")
                      (unpack "dmd-testsuite-src" "tests/d2/dmd-testsuite")))))
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
                  (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo")))
               (substitute* "tests/d2/dmd-testsuite/Makefile"
                 (("/bin/bash") (which "bash")))
               ;; FIXME: this test cannot be linked.
               (delete-file "tests/d2/dmd-testsuite/runnable/cppa.d")
               #t)))))
      (inputs
       `(("libconfig" ,libconfig)
         ("libedit" ,libedit)
         ("tzdata" ,tzdata)
         ("zlib" ,zlib)))
      (native-inputs
       `(("llvm" ,llvm)
         ("clang" ,clang)
         ("python-lit" ,python-lit)
         ("python-wrapper" ,python-wrapper)
         ("unzip" ,unzip)
         ("phobos-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/phobos/archive/ldc-v"
                   runtime-version ".tar.gz"))
             (sha256
              (base32
               "0qywnvnp019mmmr74aw90ir9f03iz0hc7cgzna609agsar0b27jl"))
             (patches (search-patches "ldc-disable-tests.patch"))))
         ("druntime-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/druntime/archive/ldc-v"
                   runtime-version ".tar.gz"))
             (sha256
              (base32
               "0z418n6x2fxac07sxpi4rl69069qiym4w6r9sjppn91q58qh8hjs"))))
         ("dmd-testsuite-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/dmd-testsuite/archive/ldc-v"
                   runtime-version ".tar.gz"))
             (sha256
              (base32
               "196mkfax5y3yqm3gz7jhqhnkjwrvr2m4a8nc9k41l0511ldzsk9x"))))))
      (home-page "http://wiki.dlang.org/LDC")
      (synopsis "LLVM compiler for the D programming language")
      (description
       "LDC is a compiler for the D programming language.  It is based on the
latest DMD frontend and uses LLVM as backend.")
      ;; Most of the code is released under BSD-3, except for code originally
      ;; written for GDC, which is released under GPLv2+, and the DMD frontend,
      ;; which is released under the "Boost Software License version 1.0".
      (license (list license:bsd-3
                     license:gpl2+
                     license:boost1.0)))))

(define-public ldc
  ;; The phobos, druntime and dmd-testsuite dependencies do not have a newer
  ;; release than 1.1.0-beta4, hence the need to make use of the older-version
  ;; variable to hold this variable.
  (let ((older-version "1.1.0"))
    (package
      (inherit ldc-bootstrap)
      (name "ldc")
      (version "1.1.1")
      ;; Beta version needed to compile various scientific tools that require
      ;; the newer beta versions, and won't compile successfully with the
      ;; older stable version.
      (source (origin
                (method url-fetch)
                (uri (string-append
                      "https://github.com/ldc-developers/ldc/archive/v"
                      version ".tar.gz"))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "0yjiwg8pnlm2286bwdkwasaqw6ys7lymrqvhh5xyb1adha1ndcav"))))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-submodule-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((unpack (lambda (source target)
                               (with-directory-excursion target
                                 (zero? (system* "tar" "xvf"
                                                 (assoc-ref inputs source)
                                                 "--strip-components=1"))))))
                 (and (unpack "phobos-src" "runtime/phobos")
                      (unpack "druntime-src" "runtime/druntime")
                      (unpack "dmd-testsuite-src" "tests/d2/dmd-testsuite")))))
           ;; The 'patch-dmd2 step in ldc causes the build to fail since
           ;; dmd2/root/port.c no longer exists.  Arguments needed to have
           ;; 'patch-dmd2 step removed, but retain everything else.
           (add-after 'unpack-submodule-sources 'patch-phobos
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "runtime/phobos/std/process.d"
                 (("/bin/sh") (which "sh"))
                 (("echo") (which "echo")))
               (substitute* "runtime/phobos/std/datetime.d"
                 (("/usr/share/zoneinfo/")
                  (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo")))
               (substitute* "tests/d2/dmd-testsuite/Makefile"
                 (("/bin/bash") (which "bash")))
               #t)))))
      (native-inputs
       `(("llvm" ,llvm)
         ("clang" ,clang)
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
               "0z5v55b9s1ppf0c2ivjq7sbmq688c37c92ihc3qwrbxnqvkkvrlk"))
             ;; This patch deactivates some tests that depend on network access
             ;; to pass.  It also deactivates some tests that have some reliance
             ;; on timezone.
             ;;
             ;; For the network tests, there's an effort to get a version flag
             ;; added to deactivate these tests for distribution packagers
             ;; that is being pursued at
             ;; <https://forum.dlang.org/post/zmdbdgnzrxyvtpqafvyg@forum.dlang.org>.
             ;; It also deactivates a test that requires /root
             (patches (search-patches "ldc-1.1.0-disable-phobos-tests.patch"))))
         ("druntime-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/druntime/archive/ldc-v"
                   older-version ".tar.gz"))
             (sha256
              (base32
               "07qvrqj6vgakd6qr4x5f70w6zwkzd1li5x8i1b5ywnds1z5lnfp6"))))
         ("dmd-testsuite-src"
          ,(origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/ldc-developers/dmd-testsuite/archive/ldc-v"
                   older-version ".tar.gz"))
             (sha256
              (base32
               "12cak7yqmsgjlflx0dp6fwmwb9dac25amgi86n0bb95ard3547wy"))
             ;; Remove the gdb tests that fails with a "Error: No such file or
             ;; directory" error, despite the files being present in the debug
             ;; files left with the --keep-failed flag to guix build.
             (patches (search-patches "ldc-1.1.0-disable-dmd-tests.patch")))))))))

(define-public ldc-beta ldc)

(define-public dub
  (package
    (name "dub")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dlang/dub/archive/"
                                  "v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1p9pmzjsmd7v3jpilv0z0c8ar1ykvri6nn5fv95f8d2vriczj29m"))))
   (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; it would have tested itself by installing some packages (vibe etc)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'build
           (lambda _
             (zero? (system* "./build.sh"))))
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
     "DUB is a package and build manager for applications and libraries written
in the D programming language.  It can automatically retrieve a project's
dependencies and integrate them in the build process.

The design emphasis is on maximum simplicity for simple projects, while
providing the opportunity to customize things when needed. ")
    (license license:expat)))
