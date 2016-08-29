;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages golang)
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
    (version "2.5.0")
    (source
     (origin
      (method url-fetch)
      (uri (list
            (string-append "http://embed.cs.utah.edu/creduce/"
                           "creduce-" version ".tar.gz")))
      (sha256
       (base32
        "1r23lhzq3dz8vi2dalxk5las8bf0av2w94hxxbs61pr73m77ik9d"))))
    (build-system gnu-build-system)
    (inputs
     `(("astyle"          ,astyle)
       ("llvm"            ,llvm)
       ("clang"           ,clang)
       ("flex"            ,flex)
       ("indent"          ,indent)
       ("perl"            ,perl)
       ("exporter-lite"   ,perl-exporter-lite)
       ("file-which"      ,perl-file-which)
       ("getopt-tabular"  ,perl-getopt-tabular)
       ("regex-common"    ,perl-regexp-common)
       ("sys-cpu"         ,perl-sys-cpu)
       ("term-readkey"    ,perl-term-readkey)))
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
                              '("term-readkey"    "exporter-lite"
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

(define qemu-2.3.0
  (package
    (inherit qemu-minimal)
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://wiki.qemu-project.org/download/qemu-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "120m53c3p28qxmfzllicjzr8syjv6v4d9rsyrgkp7gnmcgvvgfmn"))))
    (arguments
     ;; XXX: Disable tests because of GTester's rejection of duplicate test
     ;; names, which wasn't addressed in this version of QEMU.
     `(#:tests? #f
       ,@(substitute-keyword-arguments (package-arguments qemu-minimal)
           ((#:phases phases)
            ;; We disable the tests so we skip the phase disabling the qga test.
            `(modify-phases ,phases (delete 'disable-test-qga))))))))

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
      (version "2.15b")             ;It seems all releases have the 'b' suffix
      (source
       (origin
         (method url-fetch)
         (uri (string-append "http://lcamtuf.coredump.cx/afl/releases/"
                             "afl-" version ".tgz"))
         (sha256
          (base32
           "04n2jfkchpz6a07w694b0im1vcmc3220ryqcaasa7vix7784wzs2"))))
      (build-system gnu-build-system)
      (inputs
       `(("custom-qemu"
          ;; The afl-qemu tool builds qemu 2.3.0 with a few patches applied.
          ,(package (inherit qemu-2.3.0)
             (name "afl-qemu")
             (inputs
              `(("afl-src" ,source)
                ,@(package-inputs qemu-2.3.0)))
             ;; afl only supports using a single afl-qemu-trace executable, so
             ;; we only build qemu for the native target.
             (arguments
              `(#:modules ((srfi srfi-1)
                           ,@%gnu-build-system-modules)
                ,@(substitute-keyword-arguments (package-arguments qemu-2.3.0)
                    ((#:configure-flags config-flags)
                     ``(,(string-append "--target-list=" ,machine "-linux-user")
                        ,@(remove (λ (f) (string-prefix? "--target-list=" f))
                                  ,config-flags)))
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
                         (symlink (string-append qemu "/bin/qemu-" ,machine)
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

(define-public stress-make
  (let ((commit "506e6cfd98d165f22bee91c408b7c20117a682c4")
        (revision "0"))                 ;No official source distribution
    (package
      (name "stress-make")
      (version (string-append "1.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/losalamos/stress-make.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1j330yqhc7plwin04qxbh8afpg5nfnw1xvnmh8rk6mmqg9w6ik70"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("go" ,go)))
      (inputs
       `(("make-source" ,(package-source gnu-make))))
      (arguments
       ;; stress-make's configure script insists on having a tarball and does
       ;; not accept a directory name instead.  To let the gnu-build-system's
       ;; patch-* phases work properly, we unpack the source first, then
       ;; repack before the configure phase.
       `(#:configure-flags '("--with-make-tar=./make.tar.xz")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-make
             (lambda* (#:key inputs #:allow-other-keys)
               (zero? (system* "tar" "xf" (assoc-ref inputs "make-source")))))
           (add-before 'configure 'repack-make
             (lambda _
               (zero? (system* "tar" "cJf" "./make.tar.xz"
                               (string-append "make-"
                                              ,(package-version gnu-make))))))
           (add-before 'configure 'bootstrap
             (lambda _
               (zero? (system* "autoreconf" "-vfi")))))))
      (home-page "https://github.com/losalamos/stress-make")
      (synopsis "Expose race conditions in Makefiles")
      (description
       "Stress Make is a customized GNU Make that explicitely managess the
order in which concurrent jobs are run in order to provoke erroneous behavior
into becoming manifest.  It can run jobs in the order they're launched, in
backwards order, or in random order.  The thought is that if code builds
correctly with Stress Make then it is likely that the @code{Makefile} contains
no race conditions.")
      ;; stress-make wrapper is under BSD-3-modifications-must-be-indicated,
      ;; and patched GNU Make is under its own license.
      (license (list (non-copyleft "COPYING.md")
                     (package-license gnu-make))))))
