;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2019 Hartmut Goebel <h.goebel@goebel-consult.de>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
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

(define-module (gnu packages code)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

;;; Tools to deal with source code: metrics, cross-references, etc.

(define-public cflow
  (package
    (name "cflow")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/cflow/cflow-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1mzd3yf0dfv8h2av5vsxxlhpk21nw064h91b2kgfrdz92r0pnj1l"))))
    (build-system gnu-build-system)

    ;; Needed to have cflow-mode.el installed.
    (native-inputs `(("emacs" ,emacs-minimal)))
    (arguments
     '(#:configure-flags (list (string-append "CPPFLAGS="
                                              "-D" "CFLOW_PREPROC=\\\""
                                              (assoc-ref %build-inputs "gcc")
                                              "/bin/cpp\\\""))))
    (home-page "https://www.gnu.org/software/cflow/")
    (synopsis "Create a graph of control flow within a program")
    (description
     "GNU cflow analyzes C source files and produces a graph charting the
control flow of the program.  It can output the graph in several styles and
in either the POSIX format or in an extended GNU format.  cflow also includes
a major mode for Emacs for examining the flowcharts that it produces.")
    (license license:gpl3+)))

(define-public complexity
  (package
    (name "complexity")
    (version "1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/complexity/complexity-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0lr0l9kj2w3jilz9h9y4np9pf9i9ccpy6331lanki2fnz4z8ldvd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("texinfo" ,texinfo)
       ("autogen" ,autogen)))
    (home-page "https://www.gnu.org/software/complexity/")
    (synopsis "Analyze complexity of C functions")
    (description
     "GNU complexity provides tools for finding procedures that are
convoluted, overly long or otherwise difficult to understand.  This
may help in learning or reviewing unfamiliar code or perhaps
highlighting your own code that seemed comprehensible when you wrote it.")
    (license license:gpl3+)))

(define-public global                             ; a global variable
  (package
    (name "global")
    (version "6.6.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/global/global-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "10vvsgx8v54whb4j9mk5qqyb5h3rdd9da0il3wir8pcpksyk0dww"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("libltdl" ,libltdl)
              ("sqlite" ,sqlite)
              ("python-wrapper" ,python-wrapper)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-ncurses="
                            (assoc-ref %build-inputs "ncurses"))
             (string-append "--with-sqlite3="
                            (assoc-ref %build-inputs "sqlite"))
             "--disable-static")

       #:phases
       (modify-phases %standard-phases
        (add-after 'install 'post-install
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Install the plugin files in the right place.
            (let* ((out  (assoc-ref outputs "out"))
                   (data (string-append out "/share/gtags"))
                   (vim  (string-append out "/share/vim/vimfiles/plugin"))
                   (lisp (string-append out "/share/emacs/site-lisp")))
              (mkdir-p lisp)
              (mkdir-p vim)
              (rename-file (string-append data "/gtags.el")
                           (string-append lisp "/gtags.el"))
              (rename-file (string-append data "/gtags.vim")
                           (string-append vim "/gtags.vim"))
              (rename-file (string-append data "/gtags-cscope.vim")
                           (string-append vim "/gtags-cscope.vim"))
              #t))))))
    (home-page "https://www.gnu.org/software/global/")
    (synopsis "Cross-environment source code tag system")
    (description
     "GNU GLOBAL is a source code tagging system that functions in the same
way across a wide array of environments, such as different text editors,
shells and web browsers.  The resulting tags are useful for quickly moving
around in a large, deeply nested project.")
    (license license:gpl3+)))

(define-public sloccount
  (package
    (name "sloccount")
    (version "2.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dwheeler.com/sloccount/sloccount-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ayiwfjdh1946asah861ah9269s5xkc8p5fv1wnxs9znyaxs4zzs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'make-dotl-files-older
                              (lambda _
                                ;; Make the '.l' files as old as the '.c'
                                ;; files to avoid triggering the rule that
                                ;; requires Flex.
                                (define ref
                                  (stat "README"))

                                (for-each (lambda (file)
                                            (set-file-time file ref))
                                          (find-files "." "\\.[chl]$"))
                                #t))
                  (add-before 'install 'make-target-directories
                              (lambda* (#:key outputs #:allow-other-keys)
                                (let ((out (assoc-ref outputs "out")))
                                  (mkdir-p (string-append out "/bin"))
                                  (mkdir-p (string-append out
                                                          "/share/man/man1"))
                                  (mkdir-p (string-append out
                                                          "/share/doc"))
                                  #t)))
                  (replace 'check
                           (lambda _
                             (setenv "HOME" (getcwd))
                             (setenv "PATH"
                                     (string-append (getcwd) ":"
                                                    (getenv "PATH")))
                             (invoke "make" "test"))))

        #:make-flags (list (string-append "PREFIX="
                                          (assoc-ref %outputs "out")))))
    (inputs `(("perl" ,perl)))
    (home-page "https://dwheeler.com/sloccount/")
    (synopsis "Count physical source lines of code (SLOC)")
    (description
     "SLOCCount is a set of the programs for counting source lines of
code (SLOC) in large software systems.  It can automatically identify and
measure a wide range of programming languages.  It automatically estimates the
effort, time, and money it would take to develop the software, using the
COCOMO model or user-provided parameters.")
    (license license:gpl2+)))

(define-public cloc
  (package
    (name "cloc")
    (version "1.88")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AlDanial/cloc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ixgswzbzv63bl50gb2kgaqr0jcicjz6w610hi9fal1i7744zraw"))))
    (build-system gnu-build-system)
    (inputs
     `(("coreutils" ,coreutils)
       ("perl" ,perl)
       ("perl-algorithm-diff" ,perl-algorithm-diff)
       ("perl-digest-md5" ,perl-digest-md5)
       ("perl-parallel-forkmanager" ,perl-parallel-forkmanager)
       ("perl-regexp-common" ,perl-regexp-common)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure)   ; nothing to configure
                  (delete 'build)       ; nothing to build
                  (replace 'install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (invoke "make" "-C" "Unix"
                                (string-append "prefix=" out)
                                (string-append "INSTALL="
                                               (assoc-ref inputs "coreutils")
                                               "/bin/install")
                                "install")
                        #t)))
                  (add-after 'install 'wrap-program
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-program (string-append out "/bin/cloc")
                          `("PERL5LIB" ":" =
                            ,(string-split (getenv "PERL5LIB") #\:)))
                        #t))))
       #:out-of-source? #t
       ;; Tests require some other packages.
       #:tests? #f))
    (home-page "https://github.com/AlDanial/cloc")
    (synopsis "Count source lines of code (SLOC) and other source code metrics")
    (description "cloc counts blank lines, comment lines, and physical lines
of source code in many programming languages.  Given two versions of a code
base, cloc can compute differences in blank, comment, and source lines.

cloc contains code from David Wheeler's SLOCCount.  Compared to SLOCCount,
cloc can handle a greater variety of programming languages.")
    (license license:gpl2+)))

(define-public the-silver-searcher
  (package
    (name "the-silver-searcher")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://geoff.greer.fm/ag/releases/the_silver_searcher-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0w1icjqd8hd45rn1y6nbfznk1a6ip54whwbfbhxp7ws2hn3ilqnr"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("pcre" ,pcre)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (home-page "https://geoff.greer.fm/ag/")
    (synopsis "Fast code searching tool")
    (description
     "The Silver Searcher (@command{ag}) is a tool for quickly searching large
numbers of files.  It's intended primarily for source code repositories, and
respects files like @file{.gitignore} and @file{.hgignore}.  It's also an order
of magnitude faster than its inspiration, @command{ack}, and less specialised
tools such as @command{grep}.")
    (license license:asl2.0)))

(define-public trio
  (package
    (name "trio")
    (version "1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ctrio/trio/trio-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "02pwd5m5vq7hbrffgm2na1dfc249z50yyr5jv73vdw15bd7ygl44"))))
    (build-system gnu-build-system)
    (home-page "https://daniel.haxx.se/projects/trio/")
    (synopsis "Portable and extendable printf and string functions")
    (description
     "Trio is a set of @code{printf} and string functions designed be used by
applications with a focus on portability or with the need for additional
features that are not supported by the standard @code{stdio} implementation.")
    ;; This license is very similar to the ISC license, but the wording is
    ;; slightly different.
    (license (license:non-copyleft
              "http://sourceforge.net/p/ctrio/git/ci/master/tree/README"))))

(define-public universal-ctags
  (package
    (name "universal-ctags")
    (version "5.9.20201018.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/universal-ctags/ctags")
             (commit (string-append "p" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "174p1w20pl25k996hfw61inw4mqhskmmic1lyw2m65firmkyvs7x"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove the bundled PackCC and associated build rules.
           (substitute* "Makefile.am"
             (("\\$\\(packcc_verbose\\)\\$\\(PACKCC\\)")
              "packcc")
             (("\\$\\(PEG_SRCS\\) \\$\\(PEG_HEADS\\): packcc\\$\\(EXEEXT\\)")
              "$(PEG_SRCS) $(PEG_HEADS):")
             (("noinst_PROGRAMS \\+= packcc")
              ""))
           (delete-file-recursively "misc/packcc")
           #t))))
    (build-system gnu-build-system)
    (arguments
     '(;; Don't use the build-time TMPDIR (/tmp/guix-build-...) at runtime.
       #:configure-flags '("--enable-tmpdir=/tmp")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'make-files-writable
                    (lambda _
                      (for-each make-file-writable (find-files "."))
                      #t))
                  (add-before 'bootstrap 'patch-optlib2c
                    (lambda _
                      ;; The autogen.sh script calls out to optlib2c to
                      ;; generate translations, so we can not wait for the
                      ;; patch-source-shebangs phase.
                      (patch-shebang "misc/optlib2c")
                      #t))
                  (add-before 'check 'patch-tests
                    (lambda _
                      (substitute* "misc/units"
                        (("SHELL=/bin/sh")
                         (string-append "SHELL=" (which "sh"))))
                      (substitute* "Tmain/utils.sh"
                        (("/bin/echo") (which "echo")))
                      #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ;; XXX: Use ctags' own packcc fork even though we meticolously unbundle
       ;; it above.  Mainly for historical reasons, and perhaps their changes
       ;; get upstreamed in the future.
       ("packcc"
        ,(let ((commit "03402b79505dc0024f90d5bebfd7e5d3fb62da9a"))
           (package
             (inherit packcc)
             (source (origin
                       (method git-fetch)
                       (uri (git-reference
                             (url "https://github.com/universal-ctags/packcc")
                             (commit commit)))
                       (file-name (git-file-name "packcc-for-ctags"
                                                 (string-take commit 7)))
                       (sha256
                        (base32
                         "0vxpdk9l2lf7f32nx1p3b3xmw2kw2wp95vnf9bc4lyqrg69pblm0")))))))
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("jansson" ,jansson)
       ("libseccomp" ,libseccomp)
       ("libxml2" ,libxml2)
       ("libyaml" ,libyaml)))
    (home-page "https://ctags.io/")
    (synopsis "Generate tag files for source code")
    (description
     "Universal Ctags generates an index (or tag) file of language objects
found in source files for many popular programming languages.  This index
makes it easy for text editors and other tools to locate the indexed items.
Universal Ctags improves on traditional ctags because of its multilanguage
support, its ability for the user to define new languages searched by regular
expressions, and its ability to generate emacs-style TAGS files.")
    (license license:gpl2+)))

(define-public withershins
  (package
    (name "withershins")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cameronwhite/withershins")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cviqvbbcwljm1zx12j6511hazr3kscwrvcyifrkfi4fpy5z985m"))))
    (build-system cmake-build-system)
    (arguments
     `(#:out-of-source? #f
       #:configure-flags
       ;; XXX A (justified!) misleading-indentation error breaks the build.
       (list "-DENABLE_WERROR=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'find-libiberty
          (lambda _
            (let ((libiberty (assoc-ref %build-inputs "libiberty")))
              (substitute* "cmake/FindIberty.cmake"
                (("/usr/include") (string-append libiberty "/include"))
                (("libiberty.a iberty")
                 (string-append "NAMES libiberty.a iberty\nPATHS \""
                                libiberty "/lib" "\"")))
              #t)))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (include (string-append out "/include"))
                   (lib (string-append out "/lib")))
              (mkdir-p include)
              (install-file "src/withershins.hpp" include)
              (mkdir-p lib)
              (install-file "src/libwithershins.a" lib))
            #t)))))
    (home-page "https://github.com/cameronwhite/withershins")
    (inputs
     `(("libiberty" ,libiberty)
       ("binutils" ,binutils) ;for libbfd
       ("zlib" ,zlib)))
    (synopsis "C++11 library for generating stack traces")
    (description
     "Withershins is a simple cross-platform C++11 library for generating
stack traces.")
    ;; Sources are released under Expat license, but since BFD is licensed
    ;; under the GPLv3+ the combined work is GPLv3+ as well.
    (license license:gpl3+)))

(define-public lcov
  (package
    (name "lcov")
    (version "1.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/linux-test-project/lcov"
                           "/releases/download/v" version
                           "/lcov-" version ".tar.gz"))
       (sha256
        (base32 "0fh5z0q5wg2jxr2nn5w7321y0zg9rwk75j3k5hnamjdy6gxa5kf1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-pwd
           ;; Lift the requirement of having a shell in PATH.
           (lambda _
             (substitute* "bin/geninfo"
               (("qw/abs_path/")
                "qw/abs_path getcwd/"))
             (substitute* '("bin/lcov" "bin/geninfo")
               (("`pwd`")
                "getcwd()"))
             #t))
         (delete 'configure)            ;no configure script
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/geninfo")
                 `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB")))))
             #t)))))
    (inputs `(("perl" ,perl)
              ("perl-io-compress" ,perl-io-compress)
              ("perl-json" ,perl-json)))
    (home-page "http://ltp.sourceforge.net/coverage/lcov.php")
    (synopsis "Code coverage tool that enhances GNU gcov")
    (description "LCOV is an extension of @command{gcov}, a tool part of the
GNU@tie{}Binutils, which provides information about what parts of a program
are actually executed (i.e., \"covered\") while running a particular test
case.  The extension consists of a set of Perl scripts which build on the
textual @command{gcov} output to implement the following enhanced
functionality such as HTML output.")
    (license license:gpl2+)))

(define-public lcov-cobertura
  (package
    (name "python-lcov-cobertura")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lcov_cobertura" version))
       (sha256
        (base32
         "02ar6yjazlxq4p64cz9gag08bvakmzjrp147jara9wlnlbc96j8g"))))
    (build-system python-build-system)
    (home-page "https://eriwen.github.io/lcov-to-cobertura-xml/")
    (synopsis "LCOV to Cobertura XML converter")
    (description
     "The lcov-to-cobertura Python module converts code coverage report files
in the lcov format to the XML format of
@uref{http://cobertura.github.io/cobertura/, Cobertura}, a Java code coverage
tool.  It allows continuous integration servers like Jenkins to aggregate
results and determine build stability.")
    (license license:asl2.0)))

(define-public kcov
  (package
    (name "kcov")
    (version "38")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SimonKagstrom/kcov")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zqg21xwivi16csl6a5wby6679ny01bjaw4am3y4qcgjdyihifp8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;no test target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-/bin/bash-references
           (lambda _
             (substitute* (find-files "src" ".*\\.cc?$")
               (("/bin/bash") (which "bash"))
               (("/bin/sh") (which "sh")))
             #t)))))
    (inputs
     `(("curl" ,curl)
       ("elfutils" ,elfutils)
       ("libelf" ,libelf)
       ("zlib" ,zlib)))
    (native-inputs
     `(("python" ,python)))
    (home-page "https://github.com/SimonKagstrom/kcov")
    (synopsis "Code coverage tester for compiled languages, Python and Bash")
    (description "Kcov is a FreeBSD/Linux/OSX code coverage tester for compiled
languages, Python and Bash.  Kcov was originally a fork of Bcov, but has since
evolved to support a large feature set in addition to that of Bcov.

Kcov uses DWARF debugging information for compiled programs to make it
possible to collect coverage information without special compiler switches.")
    (license license:gpl2+)))

(define-public rtags
  (package
    (name "rtags")
    (version "2.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Andersbakken/rtags")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "rtags-separate-rct.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Part of spliting rct with rtags.
        ;; Substitute #include "rct/header.h" with #include <rct/header.h>.
        '(with-directory-excursion "src"
           (delete-file-recursively "rct")        ;remove bundled copy
           (let ((files (find-files "." ".*\\.cpp|.*\\.h")))
             (substitute* files
               (("#include ?\"rct/(.*.h)\"" all header)
                (string-append "#include <rct/" header ">")))
             #t)))
       (sha256
        (base32
         "0raqjbkl1ykga4ahgl9xw49cgh3cyqcf42z36z7d6fz1fw192kg0"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "RelWithDebInfo"
       #:configure-flags
       '("-DRTAGS_NO_ELISP_FILES=1"
         "-DBUILD_TESTING=FALSE")
       #:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("bash-completion" ,bash-completion)
       ("clang" ,clang)
       ("llvm" ,llvm)
       ("lua" ,lua)
       ("rct" ,rct)
       ("selene" ,selene)))
    (home-page "https://github.com/Andersbakken/rtags")
    (synopsis "Indexer for the C language family with Emacs integration")
    (description
     "RTags is a client/server application that indexes C/C++ code and keeps a
persistent file-based database of references, declarations, definitions,
symbolnames etc.  There’s also limited support for ObjC/ObjC++.  It allows you
to find symbols by name (including nested class and namespace scope).  Most
importantly we give you proper follow-symbol and find-references support.")
    (license license:gpl3+)))

(define-public colormake
  (package
    (name "colormake")
    (version "0.9.20140503")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pagekite/Colormake")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f9v5s0viq4yc9iv6701h3pv7j21zz1ckl37lpp9hsnliiizv03p"))))
    (build-system trivial-build-system)
    (native-inputs
     `(("bash" ,bash)
       ("perl" ,perl)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (copy-recursively (assoc-ref %build-inputs "source") "source")
         (chdir "source")
         (patch-shebang  "colormake.pl"
                         (list (string-append (assoc-ref %build-inputs "perl")
                                              "/bin")))
         (let* ((out (assoc-ref %outputs "out"))
                (bin (string-append out "/bin"))
                (doc (string-append out "/share/doc"))
                (install-files (lambda (files directory)
                                 (for-each (lambda (file)
                                             (install-file file directory))
                                           files))))
           (substitute* "colormake"
             (("colormake\\.pl") (string-append bin "/colormake.pl"))
             (("/bin/bash")
              (string-append (assoc-ref %build-inputs "bash") "/bin/sh")))
           (install-file "colormake.1" (string-append doc "/man/man1"))
           (install-files '("AUTHORS" "BUGS" "ChangeLog" "README") doc)
           (install-files '("colormake" "colormake-short" "clmake"
                            "clmake-short" "colormake.pl")
                          bin)
           #t))))
    (home-page "https://bre.klaki.net/programs/colormake/")
    (synopsis "Wrapper around @command{make} to produce colored output")
    (description "This package provides a wrapper around @command{make} to
produce colored output.")
    (license license:gpl2+)))

(define-public makefile2graph
  (package
    (name "makefile2graph")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lindenb/makefile2graph")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gjfk3d8qg3cla7qd2y7r9s03whlfwy83q8k76xfcnqrjjfavdgk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:make-flags (list "CC=gcc" (string-append "prefix=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("graphviz" ,graphviz)))
    (home-page "https://github.com/lindenb/makefile2graph")
    (synopsis "Creates a graph of dependencies from GNU Make")
    (description
     "@code{make2graph} creates a graph of dependencies from GNU Make.  The
output is a graphviz-dot file, a Gexf-XML file or a list of the deepest
independent targets.")
    (license license:expat)))

(define-public uncrustify
  (package
    (name "uncrustify")
    (version "0.69.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/uncrustify/uncrustify/")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sqrg13kp8fwymq40976bq380bzw40g4ss7ihlbq45d0f90ifa1k"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("unzip" ,unzip)
       ("python" ,python-wrapper)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-etc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Configuration samples are not installed by default.
             (let* ((output (assoc-ref outputs "out"))
                    (etcdir (string-append output "/etc")))
               (for-each (lambda (l)
                           (install-file l etcdir))
                         (find-files "etc" "\\.cfg$")))
             #t)))))
    (home-page "http://uncrustify.sourceforge.net/")
    (synopsis "Code formatter for C and other related languages")
    (description
     "Beautify source code in many languages of the C family (C, C++, C#,
Objective@tie{}C, D, Java, Pawn, and Vala).  Features:
@itemize
@item Indent and align code.
@item Reformat comments (a little bit).
@item Fix inter-character spacing.
@item Add or remove parens / braces.
@item Supports embedded SQL @code{EXEC SQL} stuff.
@item Highly configurable - More than 600 configurable options.
@end itemize\n")
    (license license:gpl2+)))

(define-public astyle
  (package
    (name "astyle")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/astyle/astyle/astyle%20"
                           version "/astyle_"  version "_linux.tar.gz"))
       (sha256
        (base32
         "1ms54wcs7hg1bsywqwf2lhdfizgbk7qxc9ghasxk8i99jvwlrk6b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags (list (string-append "prefix=" %output)
                          "INSTALL=install"
                          "all")
       #:modules ((guix build gnu-build-system) ;; FIXME use %default-modules
                  (guix build utils)
                  (ice-9 regex))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _ (chdir "build/gcc") #t))
         (add-after 'install 'install-libs
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Libraries and includes are not installed by default
             (let* ((output (assoc-ref outputs "out"))
                    (incdir (string-append output "/include"))
                    (libdir (string-append output "/lib")))
               (define (make-so-link sofile strip-pattern)
                 (symlink
                  (basename sofile)
                  (regexp-substitute #f
                                     (string-match strip-pattern sofile)
                                     'pre)))
               (mkdir-p incdir)
               (copy-file "../../src/astyle.h"
                          (string-append incdir "/astyle.h"))
               (mkdir-p libdir)
               (for-each (lambda (l)
                           (copy-file
                            l (string-append libdir "/" (basename l))))
                         (find-files "bin" "lib*"))
               (for-each
                (lambda (sofile)
                  (make-so-link sofile "(\\.[0-9]){3}$")  ;; link .so
                  (make-so-link sofile "(\\.[0-9]){2}$")) ;; link .so.3
                (find-files libdir "lib.*\\.so\\..*")))
             #t)))))
    (home-page "http://astyle.sourceforge.net/")
    (synopsis "Source code indenter, formatter, and beautifier")
    (description
     "Artistic Style is a source code indenter, formatter, and beautifier for
the C, C++, C++/CLI, Objective‑C, C#, and Java programming languages.")
    (license license:lgpl3+)))

(define-public indent
  (package
   (name "indent")
   (version "2.2.12")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/indent/indent-" version
                                ".tar.gz"))
            (sha256
             (base32 "12xvcd16cwilzglv9h7sgh4h1qqjd1h8s48ji2dla58m4706hzg7"))))
   (build-system gnu-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'fix-docdir
          (lambda _
            ;; Although indent uses a modern autoconf in which docdir
            ;; defaults to PREFIX/share/doc, the doc/Makefile.am
            ;; overrides this to be in PREFIX/doc.  Fix this.
            (substitute* "doc/Makefile.in"
              (("^docdir = .*$") "docdir = @docdir@\n"))
            #t))
        (add-after 'unpack 'fix-configure
          (lambda* (#:key inputs native-inputs #:allow-other-keys)
            ;; Replace outdated config.sub and config.guess:
            (with-directory-excursion "config"
              (for-each (lambda (file)
                          (install-file
                           (string-append (assoc-ref
                                           (or native-inputs inputs) "automake")
                                          "/share/automake-"
                                          ,(version-major+minor
                                            (package-version automake))
                                          "/" file) "."))
                        '("config.sub" "config.guess")))
            #t)))))
   (native-inputs
    `(("texinfo" ,texinfo)
      ("automake" ,automake))) ; For up to date 'config.guess' and 'config.sub'.
   (synopsis "Code reformatter")
   (description
    "Indent is a program that makes source code easier to read by
reformatting it in a consistent style.  It can change the style to one of
several different styles such as GNU, BSD or K&R.  It has some flexibility to
deal with incomplete or malformed syntax.  GNU indent offers several
extensions over the standard utility.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/indent/")))

(define-public amalgamate
  (let* ((commit "c91f07eea1133aa184f652b8f1398eaf03586208")
         (revision "0")
         (version (git-version "1.1.1" revision commit)))
    (package
      (name "amalgamate")
      (version version)
      (home-page "https://github.com/edlund/amalgamate")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (sha256
          (base32
           "0cllaraw8mxs8q2nr28nhgzkb417gj2wcklqg59w84f4lc78k3yb"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          '(substitute* "test.sh"
             (("test_command \"cc -Wall -Wextra -o source.out source.c\"" all)
              "test_command \"gcc -Wall -Wextra -o source.out source.c\"")))))
      (build-system gnu-build-system)
      (inputs
       `(("python" ,python-wrapper)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "amalgamate.py" bin))))
           (replace 'check
             (lambda _
               (invoke "./test.sh"))))))
      (synopsis "Tool for amalgamating C source and header files")
      ;; The package is indeed a script file, and the term "amalgamate.py" is
      ;; used by upstream.
      (description "amalgamate.py aims to make it easy to use SQLite-style C
source and header amalgamation in projects.")
      (license license:bsd-3))))
