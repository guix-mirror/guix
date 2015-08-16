;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages debug)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages indent)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages qemu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public delta
  (package
    (name "delta")
    (version "2006.08.03")
    (source
     (origin
      (method url-fetch)
      (uri (list
            (string-append "http://ftp.de.debian.org/debian/pool/main/d/delta/"
                           "delta_" version ".orig.tar.gz")
            ;; This uri seems to send guix download into an infinite loop
            (string-append "http://delta.tigris.org/files/documents/3103/"
                           "33566/delta-" version ".tar.gz")))
      (sha256
       (base32
        "184wh35pf2ddx97319s6sgkzpz48xxkbwzcjpycv009bm53lh61q"))))
    (build-system gnu-build-system)
    (inputs                             ;Installed programs are perl scripts
     `(("perl" ,perl)))
    (arguments
     `(#:phases
       (alist-replace
        'install
        (lambda* (#:key outputs #:allow-other-keys)
          ;; Makefile contains no install target
          (let* ((out (assoc-ref outputs "out"))
                 (bin (string-append out "/bin"))
                 (doc (string-append out "/share/doc/delta-" ,version)))
            (begin
              (mkdir-p bin)
              (mkdir-p doc)
              (for-each (lambda (h)
                          (copy-file h (string-append doc "/" (basename h))))
                        `("License.txt" ,@(find-files "www" ".*\\.html")))
              (for-each (lambda (b)
                          (copy-file b (string-append bin "/" b)))
                        `("delta" "multidelta" "topformflat")))))
        (alist-delete 'configure %standard-phases))))
    (home-page "http://delta.tigris.org/")
    (synopsis "Heuristical file minimizer")
    (description
     "Delta assists you in minimizing \"interesting\" files subject to a test
of their interestingness.  A common such situation is when attempting to
isolate a small failure-inducing substring of a large input that causes your
program to exhibit a bug.")
    ;; See License.txt, which is a bsd-3 license, despite the project's
    ;; home-page pointing to a bsd-2 license.
    (license bsd-3)))

(define-public c-reduce
  (package
    (name "c-reduce")
    (version "2.3.0")
    (source
     (origin
      (method url-fetch)
      (uri (list
            (string-append "http://embed.cs.utah.edu/creduce/"
                           "creduce-" version ".tar.gz")))
      (sha256
       (base32
        "0r9lvnifjcnsrkrk8k4mha1kmmb93jya7alm523ck59y3173bpi0"))
      (modules '((guix build utils)))
      (snippet
       '(substitute* "clang_delta/TransformationManager.cpp"
          (("llvm/Config/config.h") "llvm/Config/llvm-config.h")))))
    (build-system gnu-build-system)
    (inputs
     `(("astyle"          ,astyle)
       ("delta"           ,delta)
       ("llvm"            ,llvm)
       ("clang"           ,clang)
       ("flex"            ,flex)
       ("indent"          ,indent)
       ("perl"            ,perl)
       ("benchmark-timer" ,perl-benchmark-timer)
       ("exporter-lite"   ,perl-exporter-lite)
       ("file-which"      ,perl-file-which)
       ("getopt-tabular"  ,perl-getopt-tabular)
       ("regex-common"    ,perl-regexp-common)
       ("sys-cpu"         ,perl-sys-cpu)))
    (arguments
     `(#:phases (alist-cons-after
                 'install 'set-load-paths
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; Tell creduce where to find the perl modules it needs.
                   (let* ((out (assoc-ref outputs "out"))
                          (prog (string-append out "/bin/creduce")))
                     (wrap-program
                      prog
                      `("PERL5LIB" ":" prefix
                        ,(map (lambda (p)
                                (string-append (assoc-ref inputs p)
                                               "/lib/perl5/site_perl/"
                                               ,(package-version perl)))
                              '("benchmark-timer" "exporter-lite"
                                "file-which"      "getopt-tabular"
                                "regex-common"    "sys-cpu"))))))
                 %standard-phases)))
    (home-page "http://embed.cs.utah.edu/creduce")
    (synopsis "Reducer for interesting code")
    (description
     "C-Reduce is a tool that takes a large C or C++ program that has a
property of interest (such as triggering a compiler bug) and automatically
produces a much smaller C/C++ program that has the same property.  It is
intended for use by people who discover and report bugs in compilers and other
tools that process C/C++ code.")
    (license ncsa)))

(define-public american-fuzzy-lop
  (let ((machine (match (or (%current-target-system)
                            (%current-system))
                   ("x86_64-linux"   "x86_64")
                   ("i686-linux"     "i386")
                   ;; Prevent errors when querying this package on unsupported
                   ;; platforms, e.g. when running "guix package --search="
                   (_                "UNSUPPORTED"))))
    (package
      (name "american-fuzzy-lop")
      (version "1.86b")             ;It seems all releases have the 'b' suffix
      (source
       (origin
         (method url-fetch)
         (uri (string-append "http://lcamtuf.coredump.cx/afl/releases/"
                             "afl-" version ".tgz"))
         (sha256
          (base32
           "1by9ncf6lgcyibzqwyla34jv64sd66mn8zhgjz2pcgsds51qwn0r"))))
      (build-system gnu-build-system)
      (inputs
       `(("custom-qemu"
          ;; The afl-qemu tool builds qemu 2.3.0 with a few patches applied.
          ,(package (inherit qemu-headless)
             (name "afl-qemu")
             (inputs
              `(("afl-src" ,source)
                ,@(package-inputs qemu-headless)))
             ;; afl only supports using a single afl-qemu-trace executable, so
             ;; we only build qemu for the native target.
             (arguments
              `(#:configure-flags
                (list (string-append "--target-list=" ,machine "-linux-user"))
                #:modules ((srfi srfi-1)
                           ,@%gnu-build-system-modules)
                ,@(substitute-keyword-arguments (package-arguments qemu-headless)
                    ((#:phases qemu-phases)
                     `(modify-phases ,qemu-phases
                        (add-after
                         'unpack 'apply-afl-patches
                         (lambda* (#:key inputs #:allow-other-keys)
                           (let* ((afl-dir (string-append "afl-" ,version))
                                  (patch-dir
                                   (string-append afl-dir
                                                  "/qemu_mode/patches")))
                             (unless (zero?
                                      (system* "tar" "xf"
                                               (assoc-ref inputs "afl-src")))
                               (error "tar failed to unpack afl-src"))
                             (copy-file (string-append patch-dir
                                                       "/afl-qemu-cpu-inl.h")
                                        "./afl-qemu-cpu-inl.h")
                             (copy-file (string-append afl-dir "/config.h")
                                        "./afl-config.h")
                             (copy-file (string-append afl-dir "/types.h")
                                        "./types.h")
                             (substitute* "afl-qemu-cpu-inl.h"
                               (("\\.\\./\\.\\./config.h") "afl-config.h"))
                             (substitute* (string-append patch-dir
                                                         "/cpu-exec.diff")
                               (("\\.\\./patches/") ""))
                             (every (lambda (patch-file)
                                      (zero? (system* "patch" "--force" "-p1"
                                                      "--input" patch-file)))
                                    (find-files patch-dir
                                                "\\.diff$"))))))))))))))
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                            "CC=gcc")
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    (add-after
                     ;; TODO: Build and install the afl-llvm tool.
                     'install 'install-qemu
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let ((qemu (assoc-ref inputs "custom-qemu"))
                             (out  (assoc-ref outputs "out")))
                         (copy-file (string-append qemu "/bin/qemu-" ,machine)
                                    (string-append out "/bin/afl-qemu-trace"))
                         #t)))
                    (delete 'check))))
      (supported-systems (fold delete
                               %supported-systems
                               '("armhf-linux" "mips64el-linux")))
      (home-page "http://lcamtuf.coredump.cx/afl")
      (synopsis "Security-oriented fuzzer")
      (description
       "American fuzzy lop is a security-oriented fuzzer that employs a novel
type of compile-time instrumentation and genetic algorithms to automatically
discover clean, interesting test cases that trigger new internal states in the
targeted binary.  This substantially improves the functional coverage for the
fuzzed code.  The compact synthesized corpora produced by the tool are also
useful for seeding other, more labor- or resource-intensive testing regimes
down the road.")
      (license asl2.0))))
