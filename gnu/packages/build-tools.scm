;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Corentin Bocquillon <corentin@nybble.fr>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2019 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Yuval Kogman <nothingmuch@woobling.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 qblade <qblade@protonmail.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages build-tools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages ninja)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public bam
  (package
    (name "bam")
    (version "0.5.1")
    (source (origin
              ;; do not use auto-generated tarballs
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/matricks/bam")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13br735ig7lygvzyfd15fc2rdygrqm503j6xj5xkrl1r7w2wipq6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags `(,(string-append "CC=" ,(cc-for-target))
                      ,(string-append "INSTALL_PREFIX="
                                      (assoc-ref %outputs "out")))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("python" ,python-2)))
    (inputs
     (list lua))
    (home-page "https://matricks.github.io/bam/")
    (synopsis "Fast and flexible build system")
    (description "Bam is a fast and flexible build system.  Bam uses Lua to
describe the build process.  It takes its inspiration for the script files
from scons.  While scons focuses on being 100% correct when building, bam
makes a few sacrifices to acquire fast full and incremental build times.")
    (license license:bsd-3)))

(define-public bear
  (package
    (name "bear")
    (version "3.0.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rizsotto/Bear")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0klbk99qphibrp2944w8gn6x1dwwgrbm7f2bh530wjp5h3bpkr45"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-TEST_BEFORE_INSTALL
                    (lambda _
                      (substitute* "CMakeLists.txt"
                        ;; Delete the matching line—and comment out the next.
                        ((".*TEST_(BEFORE_INSTALL|COMMAND).*") "#"))))
                  (add-before 'check 'set-build-environment
                    (lambda _
                      (setenv "CC" "gcc")))
                  (replace 'check
                    ;; TODO: Test configuration is incomplete.
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "ctest")))))))
    (inputs
     `(("c-ares" ,c-ares)
       ("fmt" ,fmt)
       ("grpc" ,grpc)
       ("json-modern-cxx" ,json-modern-cxx)
       ("protobuf" ,protobuf)
       ("python" ,python-wrapper)
       ("re2" ,re2)
       ("spdlog" ,spdlog)))
    (native-inputs
     `(("abseil-cpp" ,abseil-cpp)
       ("googletest" ,googletest)
       ("openssl" ,openssl)
       ("pkg-config" ,pkg-config)
       ("python-lit" ,python-lit)
       ("zlib" ,zlib)))
    (home-page "https://github.com/rizsotto/Bear")
    (synopsis "Tool for generating a compilation database")
    (description "A JSON compilation database is used in the Clang project to
provide information on how a given compilation unit is processed.  With this,
it is easy to re-run the compilation with alternate programs.  Bear is used to
generate such a compilation database.")
    (license license:gpl3+)))

(define-public bmake
  (package
    (name "bmake")
    (version "20211212")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.crufty.net/ftp/pub/sjg/bmake-" version ".tar.gz"))
       (sha256
        (base32 "17lywks7fy5538vwyyvbvxcq5mgnd5si7f2qgw85sgqj7mdr4xdd"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal))
    (native-inputs
     (list coreutils))
    (arguments
     `(#:tests? #f                      ; test during build
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'fix-test ; fix from nixpkgs
           (lambda _
             (substitute* "unit-tests/unexport-env.mk"
               (("PATH=\t/bin:/usr/bin:/sbin:/usr/sbin")
                "PATH := ${PATH}"))))
         (add-after 'configure 'remove-fail-tests
           (lambda _
             (substitute* "unit-tests/Makefile"
               (("cmd-interrupt") "")
               (("varmod-localtime") "")))))
       #:configure-flags
       (list
        (string-append
         "--with-defshell=" (assoc-ref %build-inputs "bash") "/bin/bash")
        (string-append
         "--with-default-sys-path=" (assoc-ref %outputs "out") "/share/mk"))
       #:make-flags
       (list "INSTALL=install"))) ;; use coreutils install
    (home-page "http://www.crufty.net/help/sjg/bmake.htm")
    (synopsis "BSD's make")
    (description
     "bmake is a program designed to simplify the maintenance of other
programs.  Its input is a list of specifications as to the files upon which
programs and other files depend.")
    (license license:bsd-3)))

(define-public gn
  (let ((commit "e327ffdc503815916db2543ec000226a8df45163")
        (revision "1819"))            ;as returned by `git describe`, used below
    (package
      (name "gn")
      (version (git-version "0.0" revision commit))
      (home-page "https://gn.googlesource.com/gn")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "0kvlfj3www84zp1vmxh76x8fdjm9hyk8lkh2vdsidafpmm75fphr"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-before 'configure 'set-build-environment
                      (lambda _
                        (setenv "CC" "gcc") (setenv "CXX" "g++")
                        (setenv "AR" "ar")))
                    (replace 'configure
                      (lambda _
                        (invoke "python" "build/gen.py"
                                "--no-last-commit-position")))
                    (add-after 'configure 'create-last-commit-position
                      (lambda _
                        ;; Create "last_commit_position.h" to avoid a dependency
                        ;; on 'git' (and the checkout..).
                        (call-with-output-file "out/last_commit_position.h"
                          (lambda (port)
                            (format port
                                    (string-append
                                     "#define LAST_COMMIT_POSITION_NUM ~a\n"
                                     "#define LAST_COMMIT_POSITION \"~a (~a)\"\n")
                                    ,revision ,revision ,(string-take commit 8))))))
                    (replace 'build
                      (lambda _
                        (invoke "ninja" "-C" "out" "gn"
                                "-j" (number->string (parallel-job-count)))))
                    (replace 'check
                      (lambda* (#:key tests? #:allow-other-keys)
                        (if tests?
                            (begin
                              (invoke "ninja" "-C" "out" "gn_unittests"
                                      "-j" (number->string (parallel-job-count)))
                              (invoke "./out/gn_unittests"))
                            (format #t "test suite not run~%"))))
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((out (assoc-ref outputs "out")))
                          (install-file "out/gn" (string-append out "/bin"))))))))
      (native-inputs
       `(("ninja" ,ninja)
         ("python" ,python-wrapper)))
      (synopsis "Generate Ninja build files")
      (description
       "GN is a tool that collects information about a project from @file{.gn}
files and generates build instructions for the Ninja build system.")
      ;; GN is distributed as BSD-3, but bundles some files from ICU using the
      ;; X11 license.
      (license (list license:bsd-3 license:x11)))))

(define-public meson
  (package
    (name "meson")
    (version "0.60.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mesonbuild/meson/"
                                  "releases/download/" version  "/meson-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "13mrrizg4vl6n5k7fz6amyafnn3i097dcarr552qc0ca6nlmzjl7"))
              (patches (search-patches
                        "meson-allow-dirs-outside-of-prefix.patch"))))
    (build-system python-build-system)
    (arguments
     `(;; FIXME: Tests require many additional inputs and patching many
       ;; hard-coded file system locations in "run_unittests.py".
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  ;; Meson calls the various executables in out/bin through the
                  ;; Python interpreter, so we cannot use the shell wrapper.
                  (replace 'wrap
                    (lambda* (#:key outputs inputs #:allow-other-keys)
                      (let ((python-version
                             (python-version (assoc-ref inputs "python")))
                            (output (assoc-ref outputs "out")))
                        (substitute* (string-append output "/bin/meson")
                          (("# EASY-INSTALL-ENTRY-SCRIPT")
                           (format #f "\
import sys
sys.path.insert(0, '~a/lib/python~a/site-packages')
# EASY-INSTALL-ENTRY-SCRIPT"
                                   output python-version)))))))))
    (inputs (list python-wrapper ninja))
    (home-page "https://mesonbuild.com/")
    (synopsis "Build system designed to be fast and user-friendly")
    (description
     "The Meson build system is focused on user-friendliness and speed.
It can compile code written in C, C++, Fortran, Java, Rust, and other
languages.  Meson provides features comparable to those of the
Autoconf/Automake/make combo.  Build specifications, also known as @dfn{Meson
files}, are written in a custom domain-specific language (@dfn{DSL}) that
resembles Python.")
    (license license:asl2.0)))

;;; This older Meson variant is kept for now for gtkmm and others that may
;;; have problems with 0.60.
(define-public meson-0.59
  (package/inherit meson
    (version "0.59.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mesonbuild/meson/"
                                  "releases/download/" version  "/meson-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "117cm8794h291lca1wljz1pwnzidgbvrpg3mw3np6ksma368hyd7"))
              (patches (search-patches
                        "meson-allow-dirs-outside-of-prefix.patch"))))))

(define-public premake4
  (package
    (name "premake")
    (version "4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/premake/Premake/"
                                  version "/premake-" version "-src.zip"))
              (sha256
               (base32
                "1017rd0wsjfyq2jvpjjhpszaa7kmig6q1nimw76qx3cjz2868lrn"))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip)) ; for unpacking the source
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target)))
       #:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "build/gmake.unix") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "../../bin/release/premake4"
                           (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (synopsis "Portable software build tool")
    (description "@code{premake4} is a command line utility that reads a
scripted definition of a software project and outputs @file{Makefile}s or
other lower-level build files.")
    (home-page "https://premake.github.io")
    (license license:bsd-3)))

(define-public premake5
  (package
    (inherit premake4)
    (version "5.0.0-alpha15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/premake/premake-core/"
                                  "releases/download/v" version
                                  "/premake-" version "-src.zip"))
              (sha256
               (base32
                "0lyxfyqxyhjqsb3kmx1fyrxinb26i68hb7w7rg8lajczrgkmc3w8"))))
    (arguments
     (substitute-keyword-arguments (package-arguments premake4)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'enter-source
             (lambda _ (chdir "build/gmake2.unix") #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "../../bin/release/premake5"
                             (string-append (assoc-ref outputs "out") "/bin"))
               #t))))))
    (description "@code{premake5} is a command line utility that reads a
scripted definition of a software project and outputs @file{Makefile}s or
other lower-level build files.")))

(define-public tup
  (package
    (name "tup")
    (version "0.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://gittup.org/tup/releases/tup-v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0gnd2598xqgwihdkfkx7qn0q6p4n7npam1fy83mp7s04zwj99syc"))
              (patches (search-patches "tup-unbundle-dependencies.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; NOTE: Tup uses a slightly modified Lua, so it cannot be
                  ;; unbundled.  See: src/lula/tup-lua.patch
                  (delete-file-recursively "src/pcre")
                  (delete-file-recursively "src/sqlite3")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There is a bootstrap script, but it doesn't do what you think - it
         ;; builds tup.
         (delete 'bootstrap)
         (replace 'configure
           (lambda _
             (substitute* "src/tup/link.sh"
               (("`git describe`") ,version))
             (with-output-to-file "tup.config"
               (lambda _
                 (format #t "CONFIG_TUP_USE_SYSTEM_SQLITE=y~%")))
             #t))
         (delete 'check)
         (replace 'build
           (lambda _
             ;; Based on bootstrap-nofuse.sh, but with a detour to patch-shebang.
             (invoke "./build.sh")
             (invoke "./build/tup" "init")
             (invoke "./build/tup" "generate" "--verbose" "build-nofuse.sh")
             (patch-shebang "build-nofuse.sh")
             (invoke "./build-nofuse.sh")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((outdir (assoc-ref outputs "out"))
                    (ftdetect (string-append outdir
                                             "/share/vim/vimfiles/ftdetect")))
               (install-file "tup" (string-append outdir "/bin"))
               (install-file "tup.1" (string-append outdir "/share/man/man1"))
               (install-file "contrib/syntax/tup.vim"
                             (string-append outdir "/share/vim/vimfiles/syntax"))
               (mkdir-p ftdetect)
               (with-output-to-file (string-append ftdetect "/tup.vim")
                 (lambda _
                   (display "au BufNewFile,BufRead Tupfile,*.tup setf tup")))
               #t))))))
    (inputs
     (list fuse pcre
           `(,pcre "bin") ; pcre-config
           sqlite))
    (native-inputs
     (list pkg-config))
    (home-page "http://gittup.org/tup/")
    (synopsis "Fast build system that's hard to get wrong")
    (description "Tup is a generic build system based on a directed acyclic
graphs of commands to be executed.  Tup instruments your build to detect the
exact dependencies of the commands, allowing you to take advantage of ideal
parallelism during incremental builds, and detecting any situations where
a build worked by accident.")
    (license license:gpl2)))

(define-public osc
  (package
    (name "osc")
    (version "0.172.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openSUSE/osc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sqdnkka3c6b6hwnrmlwrgy7w62cp8raq8mph9pgd2lydzzbvwlp"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-filename
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               ;; Main osc tool is renamed in spec file, not setup.py, let's
               ;; do that too.
               (rename-file
                (string-append bin "osc-wrapper.py")
                (string-append bin "osc"))
               #t))))))
    (native-inputs
     (list python-chardet))
    (inputs
     (list python-m2crypto python-pycurl rpm))                   ; for python-rpm
    (home-page "https://github.com/openSUSE/osc")
    (synopsis "Open Build Service command line tool")
    (description "@command{osc} is a command line interface to the Open Build
Service.  It allows you to checkout, commit, perform reviews etc.  The vast
majority of the OBS functionality is available via commands and the rest can
be reached via direct API calls.")
    (license license:gpl2+)))

(define-public compiledb
  (package
    (name "compiledb")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "compiledb" version))
        (sha256
          (base32 "0vlngsdxfakyl8b7rnvn8h3l216lhbrrydr04yhy6kd03zflgfq6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'no-compat-shim-dependency
           ;; shutilwhich is only needed for python 3.3 and earlier
           (lambda _
             (substitute* "setup.py" (("^ *'shutilwhich'\n") ""))
             (substitute* "compiledb/compiler.py" (("shutilwhich") "shutil")))))))
    (propagated-inputs
      (list python-bashlex python-click))
    (native-inputs
      (list python-pytest))
    (home-page
      "https://github.com/nickdiego/compiledb")
    (synopsis
      "Generate Clang JSON Compilation Database files for make-based build systems")
    (description
     "@code{compiledb} provides a @code{make} python wrapper script which,
besides executing the make build command, updates the JSON compilation
database file corresponding to that build, resulting in a command-line
interface similar to Bear.")
    (license license:gpl3)))

(define-public build
  (package
    (name "build")
    (version "0.3.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.codesynthesis.com/download/"
                           "build/" (version-major+minor version)
                           "/build-" version ".tar.bz2"))
       (sha256
        (base32 "1lx5rpnmsbip43zpp0a57sl5rm7pjb0y6i2si6rfglfp4p9d3z76"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "install_prefix=" %output))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (delete 'configure))))
    (home-page "https://www.codesynthesis.com/projects/build/")
    (synopsis "Massively-parallel build system implemented on top of GNU make")
    (description "Build is a massively-parallel software build system
implemented on top of GNU Make, designed with the following tasks in mind:
@itemize
@item configuration
@item building
@item testing
@item installation
@end itemize
Build has features such as:
@itemize
@item Position-independent makefiles.
@item Non-recursive multi-makefile include-based structure.
@item Leaf makefiles are full-fledged GNU makefiles, not just variable definitions.
@item Complete dependency graph.
@item Inter-project dependency tracking.
@item Extensible language/compiler framework.
@end itemize")
    (license license:gpl2+)))
