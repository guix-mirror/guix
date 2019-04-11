;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Pkill -9 <pkill9@runbox.com>
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
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages code)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages virtualization)
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
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Makefile contains no install target
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/delta-" ,version)))
               (begin
                 (for-each (lambda (h)
                             (install-file h doc))
                           `("License.txt" ,@(find-files "www" ".*\\.html")))
                 (for-each (lambda (b)
                             (install-file b bin))
                           `("delta" "multidelta" "topformflat"))))
             #t))
         (delete 'configure))))         ; no configure script
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
    (version "2.8.0")
    (source
     (origin
      (method url-fetch)
      (uri (list
            (string-append "http://embed.cs.utah.edu/creduce/"
                           "creduce-" version ".tar.gz")))
      (sha256
       (base32
        "1vqx73ymfscvlyig03972a5m7ar3gx2yv6m8c6h2mibz792j5xkp"))))
    (build-system gnu-build-system)
    (inputs
     `(("astyle"          ,astyle)
       ("llvm"            ,llvm-6)
       ("clang"           ,clang-6)
       ("flex"            ,flex)
       ("indent"          ,indent)
       ("perl"            ,perl)
       ("exporter-lite"   ,perl-exporter-lite)
       ("file-which"      ,perl-file-which)
       ("getopt-tabular"  ,perl-getopt-tabular)
       ("regex-common"    ,perl-regexp-common)
       ("term-readkey"    ,perl-term-readkey)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (with-directory-excursion "tests"
               ;; Running all tests can take a looong time, and tests 4 and 5
               ;; require frama-c or kcc.  So run just one for sanity.
               (invoke "./run_tests" "1"))))
         (add-after 'install 'set-load-paths
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
                           "regex-common")))))
             #t)))))
    (home-page "https://embed.cs.utah.edu/creduce")
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
                   ("aarch64-linux"  "aarch64")
                   ("armhf-linux"    "arm")
                   ("mips64el-linux" "mips64el")
                   ;; Prevent errors when querying this package on unsupported
                   ;; platforms, e.g. when running "guix package --search="
                   (_                "UNSUPPORTED"))))
    (package
      (name "american-fuzzy-lop")
      (version "2.52b")             ;It seems all releases have the 'b' suffix
      (source
       (origin
         (method url-fetch)
         (uri (string-append "http://lcamtuf.coredump.cx/afl/releases/"
                             "afl-" version ".tgz"))
         (sha256
          (base32
           "0ig0ij4n1pwry5dw1hk4q88801jzzy2cric6y2gd6560j55lnqa3"))))
      (build-system gnu-build-system)
      (inputs
       `(("custom-qemu"
          ;; The afl-qemu tool builds qemu 2.10.0 with a few patches applied.
          ,(package (inherit qemu-minimal-2.10)
             (name "afl-qemu")
             (inputs
              `(("afl-src" ,source)
                ,@(package-inputs qemu-minimal)))
             ;; afl only supports using a single afl-qemu-trace executable, so
             ;; we only build qemu for the native target.
             (arguments
              `(#:modules ((srfi srfi-1)
                           ,@%gnu-build-system-modules)
                ,@(substitute-keyword-arguments (package-arguments qemu-minimal)
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
                             (invoke "tar" "xf"
                                     (assoc-ref inputs "afl-src"))
                             (install-file (string-append patch-dir
                                                          "/afl-qemu-cpu-inl.h")
                                           ".")
                             (copy-file (string-append afl-dir "/config.h")
                                        "./afl-config.h")
                             (install-file (string-append afl-dir "/types.h")
                                           ".")
                             (substitute* "afl-qemu-cpu-inl.h"
                               (("\\.\\./\\.\\./config.h") "afl-config.h"))
                             (substitute* (string-append patch-dir
                                                         "/cpu-exec.diff")
                               (("\\.\\./patches/") ""))
                             (for-each (lambda (patch-file)
                                         (invoke "patch" "--force" "-p1"
                                                 "--input" patch-file))
                                       (find-files patch-dir
                                                   "\\.diff$"))
                             #t))))))))))))
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                            "CC=gcc")
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    ,@(if (string=? (%current-system) (or "x86_64-linux"
                                                          "i686-linux"))
                        '()
                        '((add-before 'build 'set-afl-flag
                            (lambda _ (setenv "AFL_NO_X86" "1") #t))
                          (add-after 'install 'remove-x86-programs
                            (lambda* (#:key outputs #:allow-other-keys)
                              (let* ((out (assoc-ref outputs "out"))
                                     (bin (string-append out "/bin/")))
                                (delete-file (string-append bin "afl-gcc"))
                                (delete-file (string-append bin "afl-g++"))
                                (delete-file (string-append bin "afl-clang"))
                                (delete-file (string-append bin "afl-clang++")))
                              #t))))
                    (add-after
                     ;; TODO: Build and install the afl-llvm tool.
                     'install 'install-qemu
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let ((qemu (assoc-ref inputs "custom-qemu"))
                             (out  (assoc-ref outputs "out")))
                         (symlink (string-append qemu "/bin/qemu-" ,machine)
                                  (string-append out "/bin/afl-qemu-trace"))
                         #t)))
                    (delete 'check)))) ; Tests are run during 'install phase.
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
       (let ((make-dir (string-append "make-" (package-version gnu-make))))
         `(#:configure-flags '("--with-make-tar=./make.tar.xz"
                               "make_cv_sys_gnu_glob=yes")
           #:phases
           (modify-phases %standard-phases
             (add-after 'unpack 'unpack-make
               (lambda* (#:key inputs #:allow-other-keys)
                 (invoke "tar" "xf" (assoc-ref inputs "make-source"))))
             (add-after 'unpack-make 'set-default-shell
               (lambda _
                 ;; Taken mostly directly from (@ (gnu packages base) gnu-make)
                 (substitute* (string-append ,make-dir "/job.c")
                   (("default_shell = .*$")
                    (format #f "default_shell = \"~a\";\n"
                            (which "sh"))))))
             (add-before 'configure 'repack-make
               (lambda _
                 (invoke "tar" "cJf" "./make.tar.xz" ,make-dir)))))))
      (home-page "https://github.com/losalamos/stress-make")
      (synopsis "Expose race conditions in Makefiles")
      (description
       "Stress Make is a customized GNU Make that explicitely manages the order
in which concurrent jobs are run to provoke erroneous behavior into becoming
manifest.  It can run jobs in the order in which they're launched, in backwards
order, or in random order.  The thought is that if code builds correctly with
Stress Make, then it is likely that the @code{Makefile} contains no race
conditions.")
      ;; stress-make wrapper is under BSD-3-modifications-must-be-indicated,
      ;; and patched GNU Make is under its own license.
      (license (list (non-copyleft "COPYING.md")
                     (package-license gnu-make))))))

(define-public zzuf
  (package
    (name "zzuf")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/samhocevar/zzuf/releases/download/v"
             version "/" name "-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1mpzjaksc2qg2hzqflf39pl06p53qam2dn3hkhkcv6p00d2n4kx3"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/samhocevar/zzuf")
    (synopsis "Transparent application input fuzzer")
    (description "Zzuf is a transparent application input fuzzer.  It works by
intercepting file operations and changing random bits in the program's
input.  Zzuf's behaviour is deterministic, making it easy to reproduce bugs.")
    (license (non-copyleft "http://www.wtfpl.net/txt/copying/"))))

(define-public scanmem
  (package
    (name "scanmem")
    (version "0.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scanmem/scanmem")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17p8sh0rj8yqz36ria5bp48c8523zzw3y9g8sbm2jwq7sc27i7s9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-gui")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-python
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "gui/GameConqueror.py"
               (("/usr/bin/env python")
                (string-append (assoc-ref %build-inputs
                                          "python-wrapper") "/bin/python")))
             #t))
         (add-after 'install 'wrap-gameconqueror
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (python-path       (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/share/gameconqueror/GameConqueror.py")
                 `("GI_TYPELIB_PATH"        ":" prefix (,gi-typelib-path))
                 `("PYTHONPATH"             ":" prefix (,python-path))))
             #t)))))
    (native-inputs
     `(("libtool" ,libtool)
       ("python-wrapper" ,python-wrapper)
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("intltool" ,intltool)
       ("automake" ,automake)
       ("autoconf" ,autoconf)))
    (inputs
     `(("readline" ,readline)))
    (propagated-inputs
     `(("python-pygobject" ,python-pygobject)))
    (home-page "https://github.com/scanmem/scanmem")
    (synopsis "Memory scanner")
    (description "Scanmem is a debugging utility designed to isolate the
address of an arbitrary variable in an executing process.  Scanmem simply
needs to be told the pid of the process and the value of the variable at
several different times.  After several scans of the process, scanmem isolates
the position of the variable and allows you to modify its value.")
    ;; The library is covered by LGPLv3 or later; the application is covered
    ;; by GPLv3 or later.
    (license (list lgpl3+ gpl3+))))
