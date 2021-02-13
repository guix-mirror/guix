;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2014 Kevin Lemonnier <lemonnierk@ulrar.net>
;;; Copyright © 2015 Jeff Mickey <j@codemac.net>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (gnu packages shells)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public dash
  (package
    (name "dash")
    (version "0.5.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://gondor.apana.org.au/~herbert/dash/files/"
                           "dash-" version ".tar.gz"))
       (sha256
        (base32 "0amwphfal2rnfa63f7qd4i9m4ckv0gm3j6jlxkmfi9x6ddkz3fb2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; The man page hails from BSD, where (d)ash is the default shell.
           ;; This isn't the case on Guix or indeed most other GNU systems.
           (substitute* "src/dash.1"
             (("the standard command interpreter for the system")
              "a command interpreter based on the original Bourne shell"))
           #t))))
    (build-system gnu-build-system)
    (inputs
     `(("libedit" ,libedit)))
    (arguments
     '(#:configure-flags '("--with-libedit")))
    (home-page "http://gondor.apana.org.au/~herbert/dash")
    (synopsis "POSIX-compliant shell optimised for size")
    (description
     "dash is a POSIX-compliant @command{/bin/sh} implementation that aims to be
as small as possible, often without sacrificing speed.  It is faster than the
GNU Bourne-Again Shell (@command{bash}) at most scripted tasks.  dash is a
direct descendant of NetBSD's Almquist Shell (@command{ash}).")
    (license (list license:bsd-3
                   license:gpl2+))))    ; mksignames.c

(define-public fish
  (package
    (name "fish")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/fish-shell/fish-shell/"
                           "releases/download/" version "/"
                           "fish-" version ".tar.gz"))
       (sha256
        (base32 "1vblmb3x2k2cb0db5jdyflppnlqsm7i6jjaidyhmvaaw7ch2gffm"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "pcre2-10.32") #t))))
    (build-system cmake-build-system)
    (inputs
     `(("fish-foreign-env" ,fish-foreign-env)
       ("ncurses" ,ncurses)
       ("pcre2" ,pcre2)      ; don't use the bundled PCRE2
       ("python" ,python)))  ; for fish_config and manpage completions
    (native-inputs
     `(("doxygen" ,doxygen)
       ; for 'fish --help'
       ("groff" ,groff)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _
             ;; some tests write to $HOME
             (setenv "HOME" (getcwd))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((coreutils (assoc-ref inputs "coreutils"))
                   (bash (assoc-ref inputs "bash")))
               ;; This test fails
               (delete-file "tests/checks/pipeline-pgroup.fish")
               ;; These try to open a terminal
               (delete-file "tests/checks/interactive.fish")
               (delete-file "tests/checks/login-interactive.fish")
               ;; These contain absolute path references
               (substitute* "src/fish_tests.cpp"
                 (("/bin/echo" echo) (string-append coreutils echo))
                 (("/bin/ca" ca) (string-append coreutils ca))
                 (("\"(/bin/c)\"" _ c) (string-append "\"" coreutils c "\""))
                 (("/bin/ls_not_a_path" ls-not-a-path)
                  (string-append coreutils ls-not-a-path))
                 (("/bin/ls" ls) (string-append coreutils ls))
                 (("(/bin/)\"" _ bin) (string-append coreutils bin "\""))
                 (("/bin -" bin) (string-append coreutils bin))
                 (((string-append
                    "do_test\\(is_potential_path\\("
                    "L\"/usr\", wds, vars, PATH_REQUIRE_DIR\\)\\);"))
                  ""))
               (substitute*
                 (append (find-files "tests" ".*\\.(in|out|err)$")
                         (find-files "tests/checks" ".*\\.fish"))
                 (("/bin/pwd" pwd) (string-append coreutils pwd))
                 (("/bin/echo" echo) (string-append coreutils echo))
                 (("/bin/sh" sh) (string-append bash sh))
                 (("/bin/ls" ls) (string-append coreutils ls)))
               (substitute* (find-files "tests" ".*\\.(in|out|err)$")
                 (("/usr/bin") (string-append coreutils "/bin")))
               #t)))
         ;; Source /etc/fish/config.fish from $__fish_sysconf_dir/config.fish.
         (add-after 'patch-tests 'patch-fish-config
           (lambda _
             (let ((port (open-file "etc/config.fish" "a")))
               (display (string-append
                         "\n\n"
                         "# Patched by Guix.\n"
                         "# Source /etc/fish/config.fish.\n"
                         "if test -f /etc/fish/config.fish\n"
                         "    source /etc/fish/config.fish\n"
                         "end\n")
                        port)
               (close-port port))
             #t))
         ;; Embed absolute paths.
         (add-before 'install 'embed-absolute-paths
           (lambda _
             (substitute* "share/functions/__fish_print_help.fish"
               (("nroff") (which "nroff")))
             #t))
         ;; Enable completions, functions and configurations in user's and
         ;; system's guix profiles by adding them to __extra_* variables.
         (add-before 'install 'patch-fish-extra-paths
           (lambda _
             (let ((port (open-file "share/__fish_build_paths.fish" "a")))
               (display
                (string-append
                 "\n\n"
                 "# Patched by Guix.\n"
                 "# Enable completions, functions and configurations in user's"
                 " and system's guix profiles by adding them to __extra_*"
                 " variables.\n"
                 "set -l __guix_profile_paths ~/.guix-profile"
                 " /run/current-system/profile\n"
                 "set __extra_completionsdir"
                 " $__guix_profile_paths\"/etc/fish/completions\""
                 " $__guix_profile_paths\"/share/fish/vendor_completions.d\""
                 " $__extra_completionsdir\n"
                 "set __extra_functionsdir"
                 " $__guix_profile_paths\"/etc/fish/functions\""
                 " $__guix_profile_paths\"/share/fish/vendor_functions.d\""
                 " $__extra_functionsdir\n"
                 "set __extra_confdir"
                 " $__guix_profile_paths\"/etc/fish/conf.d\""
                 " $__guix_profile_paths\"/share/fish/vendor_conf.d\""
                 " $__extra_confdir\n")
                port)
               (close-port port))
             #t))
         ;; Use fish-foreign-env to source /etc/profile.
         (add-before 'install 'source-etc-profile
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((port (open-file "share/__fish_build_paths.fish" "a")))
               (display
                (string-append
                 "\n\n"
                 "# Patched by Guix.\n"
                 "# Use fish-foreign-env to source /etc/profile.\n"
                 "if status is-login\n"
                 "    set fish_function_path "
                 (assoc-ref inputs "fish-foreign-env") "/share/fish/functions"
                 " $__fish_datadir/functions\n"
                 "    fenv source /etc/profile\n"
                 "    set -e fish_function_path\n"
                 "end\n")
                port)
               (close-port port))
             #t)))))
    (synopsis "The friendly interactive shell")
    (description
     "Fish (friendly interactive shell) is a shell focused on interactive use,
discoverability, and friendliness.  Fish has very user-friendly and powerful
tab-completion, including descriptions of every completion, completion of
strings with wildcards, and many completions for specific commands.  It also
has extensive and discoverable help.  A special @command{help} command gives
access to all the fish documentation in your web browser.  Other features
include smart terminal handling based on terminfo, an easy to search history,
and syntax highlighting.")
    (home-page "https://fishshell.com/")
    (license license:gpl2)))

(define-public fish-foreign-env
  (package
    (name "fish-foreign-env")
    (version "0.20190116")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oh-my-fish/plugin-foreign-env")
             (commit "dddd9213272a0ab848d474d0cbde12ad034e65bc")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00xqlyl3lffc5l0viin1nyp819wf81fncqyz87jx8ljjdhilmgbs"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (out (assoc-ref %outputs "out"))
                (func-path (string-append out "/share/fish/functions")))
           (mkdir-p func-path)
           (copy-recursively (string-append source "/functions")
                             func-path)

           ;; Embed absolute paths.
           (substitute* `(,(string-append func-path "/fenv.fish")
                          ,(string-append func-path "/fenv.apply.fish")
                          ,(string-append func-path "/fenv.main.fish"))
             (("bash")
              (string-append (assoc-ref %build-inputs "bash") "/bin/bash"))
             (("sed")
              (string-append (assoc-ref %build-inputs "sed") "/bin/sed"))
             ((" tr ")
              (string-append " " (assoc-ref %build-inputs "coreutils")
                             "/bin/tr ")))))))
    (inputs
     `(("bash" ,bash)
       ("coreutils" ,coreutils)
       ("sed" ,sed)))
    (home-page "https://github.com/oh-my-fish/plugin-foreign-env")
    (synopsis "Foreign environment interface for fish shell")
    (description "@code{fish-foreign-env} wraps bash script execution in a way
that environment variables that are exported or modified get imported back
into fish.")
    (license license:expat)))

(define-public rc
  (package
    (name "rc")
    (version "1.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rakitzis/rc")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0vj1h4pcg13vxsiydmmk87dr2sra9h4gwx0c4q6fjsiw4in78rrd"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--with-edit=gnu")
       #:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'patch-trip.rc
          (lambda _
            (substitute* "trip.rc"
              (("/bin/pwd") (which "pwd"))
              (("/bin/sh")  (which "sh"))
              (("/bin/rm")  (which "rm"))
              (("/bin\\)")  (string-append (dirname (which "rm")) ")")))
            #t)))))
    (inputs `(("readline" ,readline)
              ("perl" ,perl)))
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "Alternative implementation of the rc shell by Byron Rakitzis")
    (description
     "This is a reimplementation by Byron Rakitzis of the Plan 9 shell.  It
has a small feature set similar to a traditional Bourne shell.")
    (home-page "https://github.com/rakitzis/rc")
    (license license:zlib)))

(define-public es
  (package
    (name "es")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/wryun/es-shell/releases/"
                           "download/v" version "/es-" version ".tar.gz"))
       (sha256
        (base32
         "1fplzxc6lncz2lv2fyr2ig23rgg5j96rm2bbl1rs28mik771zd5h"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 're-enter-rootdir
           ;; The tarball has no folder.
           (lambda _
             (chdir ".."))))))
    (inputs
     `(("readline" ,readline)))
    (native-inputs
     `(("bison" ,bison)))
    (synopsis "Extensible shell with higher-order functions")
    (description
     "Es is an extensible shell.  The language was derived from the Plan 9
shell, rc, and was influenced by functional programming languages, such as
Scheme, and the Tcl embeddable programming language.  This implementation is
derived from Byron Rakitzis's public domain implementation of rc, and was
written by Paul Haahr and Byron Rakitzis.")
    (home-page "https://wryun.github.io/es-shell/")
    (license license:public-domain)))

(define-public tcsh
  (package
    (name "tcsh")
    (version "6.22.02")
    (source (origin
              (method url-fetch)
              ;; Old tarballs are moved to old/.
              (uri (list (string-append "ftp://ftp.astron.com/pub/tcsh/"
                                        "tcsh-" version ".tar.gz")
                         (string-append "ftp://ftp.astron.com/pub/tcsh/"
                                        "old/tcsh-" version ".tar.gz")))
              (sha256
               (base32
                "0nw8prz1n0lmr82wnpyhrzmki630afn7p9cfgr3vl00vr9c72a7d"))
              (patches (search-patches "tcsh-fix-autotest.patch"))
              (patch-flags '("-p0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("perl" ,perl)))
    (inputs
     `(("ncurses" ,ncurses)))
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          ,@(if (%current-target-system)
                '((add-before 'configure 'set-cross-cc
                     (lambda _
                       (substitute* "configure"
                         (("CC_FOR_GETHOST=\"cc\"")
                          "CC_FOR_GETHOST=\"gcc\""))
                       #t)))
                '())
          (add-before 'check 'patch-test-scripts
            (lambda _
              ;; Take care of pwd
              (substitute* '("tests/commands.at" "tests/variables.at")
                (("/bin/pwd") (which "pwd")))
              ;; The .at files create shell scripts without shebangs. Erk.
              (substitute* "tests/commands.at"
                (("./output.sh") "/bin/sh output.sh"))
              (substitute* "tests/syntax.at"
                (("; other_script.csh") "; /bin/sh other_script.csh"))
              ;; Now, let's generate the test suite and patch it
              (invoke "make" "tests/testsuite")

              ;; This file is ISO-8859-1 encoded.
              (with-fluids ((%default-port-encoding #f))
                (substitute* "tests/testsuite"
                  (("/bin/sh") (which "sh"))))
              #t))
          (add-after 'install 'post-install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref %outputs "out"))
                     (bin (string-append out "/bin")))
                (with-directory-excursion bin
                  (symlink "tcsh" "csh"))
                #t))))))
    (home-page "https://www.tcsh.org/")
    (synopsis "Unix shell based on csh")
    (description
     "Tcsh is an enhanced, but completely compatible version of the Berkeley
UNIX C shell (csh).  It is a command language interpreter usable both as an
interactive login shell and a shell script command processor.  It includes a
command-line editor, programmable word completion, spelling correction, a
history mechanism, job control and a C-like syntax.")
    (license license:bsd-4)))

(define-public zsh
  (package
    (name "zsh")
    (version "5.8")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                           "https://www.zsh.org/pub/zsh-" version
                           ".tar.xz")
                         (string-append
                           "https://www.zsh.org/pub/old/zsh-" version
                           ".tar.xz")))
              (sha256
               (base32
                "09yyaadq738zlrnlh1hd3ycj1mv3q5hh4xl1ank70mjnqm6bbi6w"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags
                 `("--with-tcsetpgrp"
                  "--enable-pcre"
                  "--enable-maildir-support"
                  ;; share/zsh/site-functions isn't populated
                  "--disable-site-fndir"
                  ,(string-append
                    "--enable-additional-fpath="
                    "/usr/local/share/zsh/site-functions," ; for foreign OS
                    "/run/current-system/profile/share/zsh/site-functions"))
                 #:phases
                 (modify-phases %standard-phases
                   (add-before 'configure 'fix-sh
                     (lambda _
                       ;; Some of the files are ISO-8859-1 encoded.
                       (with-fluids ((%default-port-encoding #f))
                                    (substitute*
                                        '("configure"
                                          "configure.ac"
                                          "Src/exec.c"
                                          "Src/mkmakemod.sh"
                                          "Config/installfns.sh"
                                          "Config/defs.mk.in"
                                          "Test/E01options.ztst"
                                          "Test/A05execution.ztst"
                                          "Test/A01grammar.ztst"
                                          "Test/A06assign.ztst"
                                          "Test/B02typeset.ztst"
                                          "Completion/Unix/Command/_init_d"
                                          "Util/preconfig")
                                      (("/bin/sh") (which "sh"))))))
                   (add-before 'check 'patch-test
                     (lambda _
                       ;; In Zsh, `command -p` searches a predefined set of
                       ;; paths that don't exist in the build environment. See
                       ;; the assignment of 'path' in Src/init.c'
                       (substitute* "Test/A01grammar.ztst"
                         (("command -pv") "command -v")
                         (("command -p") "command ")
                         (("'command' -p") "'command' "))
                       #t)))))
    (native-inputs `(("autoconf" ,autoconf)))
    (inputs `(("ncurses" ,ncurses)
              ("pcre" ,pcre)
              ("perl" ,perl)))
    (synopsis "Powerful shell for interactive use and scripting")
    (description "The Z shell (zsh) is a Unix shell that can be used
as an interactive login shell and as a powerful command interpreter
for shell scripting.  Zsh can be thought of as an extended Bourne shell
with a large number of improvements, including some features of bash,
ksh, and tcsh.")
    (home-page "https://www.zsh.org/")

    ;; The whole thing is under an MIT/X11-style license, but there's one
    ;; command, 'Completion/Unix/Command/_darcs', which is under GPLv2+.
    (license license:gpl2+)))

(define-public xonsh
  (package
    (name "xonsh")
    (version "0.9.25")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xonsh" version))
        (sha256
          (base32 "0q1xy0k5mvz4031p4d3y0j7ljpx32rmpk21jy148l6xcss039br0"))
        (modules '((guix build utils)))
        (snippet
         `(begin
            ;; Delete bundled PLY.
            (delete-file-recursively "xonsh/ply")
            (substitute* "setup.py"
              (("\"xonsh\\.ply\\.ply\",") ""))
            ;; Use our properly packaged PLY instead.
            (substitute* (list "setup.py"
                               "tests/test_lexer.py"
                               "xonsh/__amalgam__.py"
                               "xonsh/lexer.py"
                               "xonsh/parsers/base.py"
                               "xonsh/xonfig.py")
              (("from xonsh\\.ply\\.(.*) import" _ module)
               (format #f "from ~a import" module))
              (("from xonsh\\.ply import") "import"))
            #t))))
    (build-system python-build-system)
    (arguments
     '(;; TODO Try running run the test suite.
       ;; See 'requirements-tests.txt' in the source distribution for more
       ;; information.
       #:tests? #f))
    (inputs
     `(("python-ply" ,python-ply)))
    (home-page "https://xon.sh/")
    (synopsis "Python-ish shell")
    (description
     "Xonsh is a Python-ish, BASHwards-looking shell language and command
prompt.  The language is a superset of Python 3.4+ with additional shell
primitives that you are used to from Bash and IPython.  It works on all major
systems including Linux, Mac OSX, and Windows.  Xonsh is meant for the daily
use of experts and novices alike.")
    (license license:bsd-2)))

(define-public scsh
  (let ((commit "114432435e4eadd54334df6b37fcae505079b49f")
        (revision "1"))
    (package
      (name "scsh")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/scheme/scsh")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1ghk08akiz7hff1pndi8rmgamgcrn2mv9asbss9l79d3c2iaav3q"))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'replace-rx
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((rx (assoc-ref inputs "scheme48-rx"))
                      (rxpath (string-append rx "/share/scheme48-"
                                             ,(package-version scheme48)
                                             "/rx")))
                 (delete-file-recursively "rx")
                 (symlink rxpath "rx"))
               #t)))))
      (inputs
       `(("scheme48" ,scheme48)
         ("scheme48-rx" ,scheme48-rx)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)))
      (home-page "https://github.com/scheme/scsh")
      (synopsis "Unix shell embedded in Scheme")
      (description
       "Scsh is a Unix shell embedded in Scheme.  Scsh has two main
components: a process notation for running programs and setting up pipelines
and redirections, and a complete syscall library for low-level access to the
operating system.")
      (license license:bsd-3))))

(define-public linenoise
  (let ((commit "2105ce445821381cf1bca87b6d386d4ea88ee20d")
        (revision "1"))
    (package
      (name "linenoise")
      (version (string-append "1.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/antirez/linenoise")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1z16qwix8z6a40fskdgxsibkqgdrp4q6ncp4n6hnv4r9iihy2d8r"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no tests are included
         #:make-flags
         (list ,(string-append "CC=" (cc-for-target)))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               ;; At the moment there is no 'make install' in upstream.
               (let* ((out (assoc-ref outputs "out")))
                 (install-file "linenoise.h"
                               (string-append out "/include/linenoise"))
                 (install-file "linenoise.c"
                               (string-append out "/include/linenoise"))
                 #t))))))
      (home-page "https://github.com/antirez/linenoise")
      (synopsis "Minimal zero-config readline replacement")
      (description
       "Linenoise is a minimal, zero-config, readline replacement.
Its features include:

@enumerate
@item Single and multi line editing mode with the usual key bindings
@item History handling
@item Completion
@item Hints (suggestions at the right of the prompt as you type)
@item A subset of VT100 escapes, ANSI.SYS compatible
@end enumerate\n")
      (license license:bsd-2))))

(define-public s-shell
  (let ((commit "da2e5c20c0c5f477ec3426dc2584889a789b1659")
        (revision "2"))
    (package
      (name "s-shell")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rain-1/s")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0qiny71ww5nhzy4mnc8652hn0mlxyb67h333gbdxp4j4qxsi13q4"))))
      (build-system gnu-build-system)
      (inputs
       `(("linenoise" ,linenoise)))
      (arguments
       `(#:tests? #f
         #:make-flags (list "CC=gcc"
                            (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'install-directory-fix
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (substitute* "Makefile"
                   (("out") bin))
                 #t)))
           (add-after 'install 'manpage
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "s.1" (string-append (assoc-ref outputs "out")
                                                  "/share/man/man1"))))
           (replace 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               ;; At this point linenoise is meant to be included,
               ;; so we have to really copy it into the working directory
               ;; of s.
               (let* ((linenoise (assoc-ref inputs "linenoise"))
                      (noisepath (string-append linenoise "/include/linenoise"))
                      (out (assoc-ref outputs "out")))
                 (copy-recursively noisepath "linenoise")
                 (substitute* "s.c"
                   (("/bin/s") (string-append out "/bin/s")))
                 #t))))))
      (home-page "https://github.com/rain-1/s")
      (synopsis "Extremely minimal shell with the simplest syntax possible")
      (description
       "S is a new shell that aims to be extremely simple.  It does not
implement the POSIX shell standard.

There are no globs or \"splatting\" where a variable $FOO turns into multiple
command line arguments.  One token stays one token forever.
This is a \"no surprises\" straightforward approach.

There are no redirection operators > in the shell language, they are added as
extra programs.  > is just another unix command, < is essentially cat(1).
A @code{andglob} program is also provided along with s.")
      (license license:bsd-3))))

(define-public oksh
  (package
    (name "oksh")
    (version "0.5.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://connochaetos.org/oksh/oksh-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0ln9yf6pxngsviqszv8klnnvn8vcpplvj1njdn8xr2y8frkbw8r3"))))
    (build-system gnu-build-system)
    (arguments
     `(; The test files are not part of the distributed tarball.
       #:tests? #f))
    (home-page "https://connochaetos.org/oksh")
    (synopsis "Port of OpenBSD Korn Shell")
    (description
     "Oksh is a port of the OpenBSD Korn Shell.
The OpenBSD Korn Shell is a cleaned up and enhanced ksh.")
    (license license:gpl3+)))

(define-public loksh
  (package
    (name "loksh")
    (version "6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dimkr/loksh")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a8s64n97ikvvi7ckirxnnjvcmhr3dd4rnqm2ivapyzb0wp42jk7"))))
    (build-system gnu-build-system)
    (inputs
     `(("libbsd" ,libbsd)
       ("ncurses" ,ncurses)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #f                      ; no tests included
       #:make-flags (list "CC=gcc" "HAVE_LIBBSD=1"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (home-page "https://github.com/dimkr/loksh")
    (synopsis "Korn Shell from OpenBSD")
    (description
     "loksh is a Linux port of OpenBSD's @command{ksh}.  It is a small,
interactive POSIX shell targeted at resource-constrained systems.")
    ;; The file 'LEGAL' says it is the public domain, and the 2
    ;; exceptions which are listed are not included in this port.
    (license license:public-domain)))

(define-public mksh
  (package
    (name "mksh")
    (version "58")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.mirbsd.org/MirOS/dist/mir/mksh/mksh-R"
                           version ".tgz"))
       (sha256
        (base32 "1337zjvzh14yncg9igdry904a3ns52l8rnm1kcq262w7f5xyp2v0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; tests require access to /dev/tty
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (setenv "CC" "gcc")
             (invoke (which "sh") "Build.sh")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "mksh" bin)
               (with-directory-excursion bin
                 (symlink "mksh" "ksh"))
               (install-file "mksh.1" man)
               #t))))))
    (home-page "https://www.mirbsd.org/mksh.htm")
    (synopsis "Korn Shell from MirBSD")
    (description "mksh is an actively developed free implementation of the
Korn Shell programming language and a successor to the Public Domain Korn
Shell (pdksh).")
    (license (list license:miros
                   license:isc))))              ; strlcpy.c

(define-public oil
  (package
    (name "oil")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.oilshell.org/download/oil-"
                           version ".tar.gz"))
       (sha256
        (base32 "0yxd020jkwhhvq2l8hd9npp2vn6fvm11f0izfzwdl2npbnc976jr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:strip-binaries? #f             ; strip breaks the binary
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "CC" ,(cc-for-target))
               (substitute* "configure"
                 ((" cc ") " $CC "))
               (invoke "./configure" (string-append "--prefix=" out)
                       "--with-readline"))))
         (replace 'check
           ;; The tests are not distributed in the tarballs but upstream
           ;; recommends running this smoke test.
           ;; https://github.com/oilshell/oil/blob/release/0.8.0/INSTALL.txt#L38-L48
           (lambda _
             (let* ((oil "_bin/oil.ovm"))
               (invoke/quiet oil "osh" "-c" "echo hi")
               (invoke/quiet oil "osh" "-n" "configure")))))))
    (inputs
     `(("readline" ,readline)))
    (home-page "https://www.oilshell.org")
    (synopsis "Programming language and Bash-compatible Unix shell")
    (description "Oil is a programming language with automatic translation for
Bash.  It includes osh, a Unix/POSIX shell that runs unmodified Bash
scripts.")
    (license (list license:psfl                 ; tarball includes python2.7
                   license:asl2.0))))

(define-public oil-shell
  (deprecated-package "oil-shell" oil))

(define-public gash
  (package
    (name "gash")
    (version "0.2.0")
    (source
     (origin (method url-fetch)
             (uri (string-append "mirror://savannah/gash/gash-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "13m0yz5h9nj3x40mr6wr5xcpq1lscndfwcicw3skrz801025hhgf"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; Allow builds with Guile 3.0.
                 (substitute* "configure"
                   (("search=\"2\\.2 2\\.0\"")
                    "search=\"3.0 2.2 2.0\""))
                 #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-3.0)))
    (arguments
     '(#:make-flags '("XFAIL_TESTS=tests/redirects.org")))
    (home-page "https://savannah.nongnu.org/projects/gash/")
    (synopsis "POSIX-compatible shell written in Guile Scheme")
    (description "Gash is a POSIX-compatible shell written in Guile
Scheme.  It provides both the shell interface, as well as a Guile
library for parsing shell scripts.  Gash is designed to bootstrap Bash
as part of the Guix bootstrap process.")
    (license license:gpl3+)))

(define-public gash-utils
  (package
    (name "gash-utils")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/gash/gash-utils-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ib2p52qmbac5n0s5bys4fiwim461ps546976l1n7pwbs0avh7fk"))
              (patches (search-patches "gash-utils-ls-test.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure"
                    (("search=\"2\\.2 2\\.0\"")
                     "search=\"3.0 2.2 2.0\""))
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-3.0)
       ("gash" ,gash)))
    (home-page "https://savannah.nongnu.org/projects/gash/")
    (synopsis "Core POSIX utilities written in Guile Scheme")
    (description "Gash-Utils provides Scheme implementations of many
common POSIX utilities (there are about 40 of them, ranging in
complexity from @command{false} to @command{awk}).  The utilities are
designed to be capable of bootstrapping their standard GNU counterparts.
Underpinning these utilities are many Scheme interfaces for manipulating
files and text.")
    (license license:gpl3+)))

(define-public nushell
  ;; Regular 0.26.0 release has an issue with rust-sysinfo.  Use a later
  ;; commit including the patches fixing it.
  (let ((commit "30833468844eca336e24e02c3304a0245b5760b5")
        (revision "0"))
    (package
      (name "nushell")
      (version (git-version "0.26.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nushell/nushell.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "19miwximiqh9j7cjh2f72c38a06d0zvd9fxa7m8aaz8cbcmnsrjp"))))
      (build-system cargo-build-system)
      (arguments
       `(#:rust ,rust-1.47
         #:tests? #false                ;missing files
         #:features '("extra")
         #:cargo-inputs
         (("rust-clap" ,rust-clap-2)
          ("rust-ctrlc" ,rust-ctrlc-3)
          ("rust-futures" ,rust-futures-0.3)
          ("rust-itertools" ,rust-itertools-0.10)
          ("rust-log" ,rust-log-0.4)
          ("rust-nu-cli" ,rust-nu-cli-0.26)
          ("rust-nu-command" ,rust-nu-command-0.26)
          ("rust-nu-data" ,rust-nu-data-0.26)
          ("rust-nu-engine" ,rust-nu-engine-0.26)
          ("rust-nu-errors" ,rust-nu-errors-0.26)
          ("rust-nu-parser" ,rust-nu-parser-0.26)
          ("rust-nu-plugin" ,rust-nu-plugin-0.26)
          ("rust-nu-protocol" ,rust-nu-protocol-0.26)
          ("rust-nu-source" ,rust-nu-source-0.26)
          ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
          ("rust-nu-plugin-binaryview" ,rust-nu-plugin-binaryview-0.26)
          ("rust-nu-plugin-chart" ,rust-nu-plugin-chart-0.26)
          ("rust-nu-plugin-fetch" ,rust-nu-plugin-fetch-0.26)
          ("rust-nu-plugin-from-bson" ,rust-nu-plugin-from-bson-0.26)
          ("rust-nu-plugin-from-sqlite" ,rust-nu-plugin-from-sqlite-0.26)
          ("rust-nu-plugin-inc" ,rust-nu-plugin-inc-0.26)
          ("rust-nu-plugin-match" ,rust-nu-plugin-match-0.26)
          ("rust-nu-plugin-post" ,rust-nu-plugin-post-0.26)
          ("rust-nu-plugin-ps" ,rust-nu-plugin-ps-0.26)
          ("rust-nu-plugin-s3" ,rust-nu-plugin-s3-0.26)
          ("rust-nu-plugin-selector" ,rust-nu-plugin-selector-0.26)
          ("rust-nu-plugin-start" ,rust-nu-plugin-start-0.26)
          ("rust-nu-plugin-sys" ,rust-nu-plugin-sys-0.26)
          ("rust-nu-plugin-textview" ,rust-nu-plugin-textview-0.26)
          ("rust-nu-plugin-to-bson" ,rust-nu-plugin-to-bson-0.26)
          ("rust-nu-plugin-to-sqlite" ,rust-nu-plugin-to-sqlite-0.26)
          ("rust-nu-plugin-tree" ,rust-nu-plugin-tree-0.26)
          ("rust-nu-plugin-xpath" ,rust-nu-plugin-xpath-0.26)
          ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.4))
         #:cargo-development-inputs
         (("rust-dunce" ,rust-dunce-1)
          ("rust-nu-test-support" ,rust-nu-test-support-0.26))))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("python" ,python)))
      (inputs
       `(("curl" ,curl)
         ("libgit2" ,libgit2)
         ("libx11" ,libx11)
         ("libxcb" ,libxcb)
         ("openssl" ,openssl)
         ("zlib" ,zlib)))
      (home-page "https://www.nushell.sh")
      (synopsis "Shell that understands the structure of the data")
      (description
       "Nu draws inspiration from projects like PowerShell, functional
programming languages, and modern CLI tools.  Rather than thinking of files
and services as raw streams of text, Nu looks at each input as something with
structure.  For example, when you list the contents of a directory, what you
get back is a table of rows, where each row represents an item in that
directory.  These values can be piped through a series of steps, in a series
of commands called a ``pipeline''.")
      (license license:expat))))

(define-public rust-nu-cli-0.26
  (package
    (name "rust-nu-cli")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-cli" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1cm560qksr7j55kwfxldfp9ry25dbwnw6l3lmm3pbc0hfwdp1w0r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-arboard" ,rust-arboard-1)
        ("rust-async-recursion" ,rust-async-recursion-0.3)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-byte-unit" ,rust-byte-unit-4)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-calamine" ,rust-calamine-0.16)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-chrono-tz" ,rust-chrono-tz-0.5)
        ("rust-clap" ,rust-clap-2)
        ("rust-codespan-reporting" ,rust-codespan-reporting-0.11)
        ("rust-csv" ,rust-csv-1)
        ("rust-ctrlc" ,rust-ctrlc-3)
        ("rust-derive-new" ,rust-derive-new-0.5)
        ("rust-directories-next" ,rust-directories-next-2)
        ("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-dtparse" ,rust-dtparse-1)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-eml-parser" ,rust-eml-parser-0.1)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-filesize" ,rust-filesize-0.2)
        ("rust-fs-extra" ,rust-fs-extra-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-futures-codec" ,rust-futures-codec-0.4)
        ("rust-getset" ,rust-getset-0.1)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-htmlescape" ,rust-htmlescape-0.3)
        ("rust-ical" ,rust-ical-0.7)
        ("rust-ichwh" ,rust-ichwh-0.3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-inflector" ,rust-inflector-0.11)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-meval" ,rust-meval-0.2)
        ("rust-nu-command" ,rust-nu-command-0.26)
        ("rust-nu-data" ,rust-nu-data-0.26)
        ("rust-nu-engine" ,rust-nu-engine-0.26)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-json" ,rust-nu-json-0.26)
        ("rust-nu-parser" ,rust-nu-parser-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-stream" ,rust-nu-stream-0.26)
        ("rust-nu-table" ,rust-nu-table-0.26)
        ("rust-nu-test-support" ,rust-nu-test-support-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-num-format" ,rust-num-format-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-pin-utils" ,rust-pin-utils-0.1)
        ("rust-pretty-hex" ,rust-pretty-hex-0.2)
        ("rust-ptree" ,rust-ptree-0.3)
        ("rust-query-interface" ,rust-query-interface-0.3)
        ("rust-quick-xml" ,rust-quick-xml-0.20)
        ("rust-quickcheck-macros" ,rust-quickcheck-macros-0.9)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-roxmltree" ,rust-roxmltree-0.14)
        ("rust-rusqlite" ,rust-rusqlite-0.24)
        ("rust-rust-embed" ,rust-rust-embed-5)
        ("rust-rustyline" ,rust-rustyline-6)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-serde-ini" ,rust-serde-ini-0.2)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-shadow-rs" ,rust-shadow-rs-0.5)
        ("rust-shadow-rs" ,rust-shadow-rs-0.5)
        ("rust-shellexpand" ,rust-shellexpand-2)
        ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.1)
        ("rust-sxd-document" ,rust-sxd-document-0.3)
        ("rust-sxd-xpath" ,rust-sxd-xpath-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-term" ,rust-term-0.6)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-titlecase" ,rust-titlecase-1)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-trash" ,rust-trash-1)
        ("rust-umask" ,rust-umask-1)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-url" ,rust-url-2)
        ("rust-users" ,rust-users-0.10)
        ("rust-uuid" ,rust-uuid-0.8)
        ("rust-which" ,rust-which-4)
        ("rust-zip" ,rust-zip-0.5))))
    (home-page "https://www.nushell.sh")
    (synopsis "CLI for nushell")
    (description "CLI for nushell")
    (license license:expat)))

(define-public rust-nu-command-0.26
  (package
    (name "rust-nu-command")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-command" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0kqip12aw38c6wa9blzmk4xyndjgm4k23iiaw2187vsanh0h53q7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-arboard" ,rust-arboard-1)
        ("rust-async-recursion" ,rust-async-recursion-0.3)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-byte-unit" ,rust-byte-unit-4)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-calamine" ,rust-calamine-0.16)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-chrono-tz" ,rust-chrono-tz-0.5)
        ("rust-clap" ,rust-clap-2)
        ("rust-codespan-reporting" ,rust-codespan-reporting-0.11)
        ("rust-csv" ,rust-csv-1)
        ("rust-ctrlc" ,rust-ctrlc-3)
        ("rust-derive-new" ,rust-derive-new-0.5)
        ("rust-directories-next" ,rust-directories-next-2)
        ("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-dtparse" ,rust-dtparse-1)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-eml-parser" ,rust-eml-parser-0.1)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-filesize" ,rust-filesize-0.2)
        ("rust-fs-extra" ,rust-fs-extra-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-futures-codec" ,rust-futures-codec-0.4)
        ("rust-getset" ,rust-getset-0.1)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-htmlescape" ,rust-htmlescape-0.3)
        ("rust-ical" ,rust-ical-0.7)
        ("rust-ichwh" ,rust-ichwh-0.3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-inflector" ,rust-inflector-0.11)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-meval" ,rust-meval-0.2)
        ("rust-nu-data" ,rust-nu-data-0.26)
        ("rust-nu-engine" ,rust-nu-engine-0.26)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-json" ,rust-nu-json-0.26)
        ("rust-nu-parser" ,rust-nu-parser-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-stream" ,rust-nu-stream-0.26)
        ("rust-nu-table" ,rust-nu-table-0.26)
        ("rust-nu-test-support" ,rust-nu-test-support-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-num-format" ,rust-num-format-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-pin-utils" ,rust-pin-utils-0.1)
        ("rust-pretty-hex" ,rust-pretty-hex-0.2)
        ("rust-ptree" ,rust-ptree-0.3)
        ("rust-query-interface" ,rust-query-interface-0.3)
        ("rust-quick-xml" ,rust-quick-xml-0.20)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-regex" ,rust-regex-1)
        ("rust-roxmltree" ,rust-roxmltree-0.14)
        ("rust-rusqlite" ,rust-rusqlite-0.24)
        ("rust-rust-embed" ,rust-rust-embed-5)
        ("rust-rustyline" ,rust-rustyline-6)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-serde-ini" ,rust-serde-ini-0.2)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.7)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-shadow-rs" ,rust-shadow-rs-0.5)
        ("rust-shellexpand" ,rust-shellexpand-2)
        ("rust-strip-ansi-escapes" ,rust-strip-ansi-escapes-0.1)
        ("rust-sxd-document" ,rust-sxd-document-0.3)
        ("rust-sxd-xpath" ,rust-sxd-xpath-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-term" ,rust-term-0.6)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-titlecase" ,rust-titlecase-1)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-trash" ,rust-trash-1)
        ("rust-umask" ,rust-umask-1)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
        ("rust-url" ,rust-url-2)
        ("rust-users" ,rust-users-0.10)
        ("rust-uuid" ,rust-uuid-0.8)
        ("rust-which" ,rust-which-4)
        ("rust-zip" ,rust-zip-0.5))))
    (home-page "https://www.nushell.sh")
    (synopsis "CLI for nushell")
    (description "CLI for nushell")
    (license license:expat)))

(define-public rust-nu-data-0.26
  (package
    (name "rust-nu-data")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-data" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jmicfs49laczgvif5axv2gb0c52hrfxvf3ywi7iinr7n8h1sfpx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-byte-unit" ,rust-byte-unit-4)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-derive-new" ,rust-derive-new-0.5)
        ("rust-directories-next" ,rust-directories-next-2)
        ("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-getset" ,rust-getset-0.1)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-table" ,rust-nu-table-0.26)
        ("rust-nu-test-support" ,rust-nu-test-support-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-num-format" ,rust-num-format-0.4)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-query-interface" ,rust-query-interface-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-users" ,rust-users-0.10))))
    (home-page "https://www.nushell.sh")
    (synopsis "CLI for nushell")
    (description "CLI for nushell")
    (license license:expat)))

(define-public rust-nu-engine-0.26
  (package
    (name "rust-nu-engine")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-engine" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fbvv8dvxzfbkyvy8iyis9xvijv794lpjfp0hrl25s0lrjykbji0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-async-recursion" ,rust-async-recursion-0.3)
        ("rust-async-trait" ,rust-async-trait-0.1)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-derive-new" ,rust-derive-new-0.5)
        ("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-filesize" ,rust-filesize-0.2)
        ("rust-fs-extra" ,rust-fs-extra-1)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-futures-codec" ,rust-futures-codec-0.4)
        ("rust-getset" ,rust-getset-0.1)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-nu-data" ,rust-nu-data-0.26)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-parser" ,rust-nu-parser-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-stream" ,rust-nu-stream-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-termcolor" ,rust-termcolor-1)
        ("rust-umask" ,rust-umask-1)
        ("rust-users" ,rust-users-0.10))))
    (home-page "https://www.nushell.sh")
    (synopsis "Core commands for nushell")
    (description "Core commands for nushell")
    (license license:expat)))

(define-public rust-nu-errors-0.26
  (package
    (name "rust-nu-errors")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-errors" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q6cbb02m9cfvjc7fcfzj814iz9ab2j2m3c3qwqskkrig4cwz3sl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-codespan-reporting" ,rust-codespan-reporting-0.11)
        ("rust-derive-new" ,rust-derive-new-0.5)
        ("rust-getset" ,rust-getset-0.1)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-toml" ,rust-toml-0.5))))
    (home-page "https://www.nushell.sh")
    (synopsis "Core error subsystem for Nushell")
    (description "Core error subsystem for Nushell")
    (license license:expat)))

(define-public rust-nu-json-0.26
  (package
    (name "rust-nu-json")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "085k9p41pqzivw6z6rxxzr8haagwqi795a2dh084qs99cyi5hj2n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-regex" ,rust-regex-1)
        ("rust-serde" ,rust-serde-0.8))))
    (home-page "https://www.nushell.sh")
    (synopsis "Fork of @code{serde-hjson}")
    (description "This package is a fork of @code{serde-hjson}.")
    (license license:expat)))

(define-public rust-nu-parser-0.26
  (package
    (name "rust-nu-parser")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-parser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05wns7z1xifdlypklar2hb2q9515lrlwx5jl69aw3k877nmwg07p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-codespan-reporting" ,rust-codespan-reporting-0.11)
        ("rust-derive-new" ,rust-derive-new-0.5)
        ("rust-derive-is-enum-variant" ,rust-derive-is-enum-variant-0.1)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-test-support" ,rust-nu-test-support-0.26)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-shellexpand" ,rust-shellexpand-2)
        ("rust-smart-default" ,rust-smart-default-0.6))))
    (home-page "https://www.nushell.sh")
    (synopsis "Nushell parser")
    (description "Nushell parser")
    (license license:expat)))

(define-public rust-nu-plugin-0.26
  (package
    (name "rust-nu-plugin")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-plugin" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n7lf9d0dkjdnmwv60szrz22z47aq3yl946f9y20dnk7xy798bv6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-test-support" ,rust-nu-test-support-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1))))
    (home-page "https://www.nushell.sh")
    (synopsis "Nushell Plugin")
    (description "Nushell Plugin")
    (license license:expat)))

(define-public rust-nu-plugin-binaryview-0.26
  (package
    (name "rust-nu-plugin-binaryview")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_binaryview" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ngnkpcl6gqi3rp2k666rfq35hlqhii3jlg0yfi42inxsh5mxgpf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-crossterm" ,rust-crossterm-0.18)
        ("rust-image" ,rust-image-0.22)
        ("rust-neso" ,rust-neso-0.5)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-pretty-hex" ,rust-pretty-hex-0.2)
        ("rust-rawkey" ,rust-rawkey-0.1))))
    (home-page "https://www.nushell.sh")
    (synopsis "Binary viewer plugin for Nushell")
    (description
     "This package provides a binary viewer plugin for Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-chart-0.26
  (package
    (name "rust-nu-plugin-chart")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_chart" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pyccvidzqgz7yp16jdjgyb708cimx0j334wzi8ij8l7x395qwn3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crossterm" ,rust-crossterm-0.19)
        ("rust-nu-cli" ,rust-nu-cli-0.26)
        ("rust-nu-data" ,rust-nu-data-0.26)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-tui" ,rust-tui-0.14))))
    (home-page "https://www.nushell.sh")
    (synopsis "Plugin to display charts")
    (description
     "This package provides a plugin to display charts in Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-fetch-0.26
  (package
    (name "rust-nu-plugin-fetch")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_fetch" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f61f3n5wdzqwfi94ym8j60sd1wah1z1gdyhyg60fbyygd9scm4r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.13)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-surf" ,rust-surf-1)
        ("rust-url" ,rust-url-2))))
    (home-page "https://www.nushell.sh")
    (synopsis "URL fetch plugin for Nushell")
    (description "This package provides a URL fetch plugin for Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-from-bson-0.26
  (package
    (name "rust-nu-plugin-from-bson")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_from_bson" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1ywvxlx1i44yz7md5wbd4sadbh2j6nah6ccfpkrls0gjmagqsbvj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-bson" ,rust-bson-0.14)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://www.nushell.sh")
    (synopsis "Converter plugin to the bson format for Nushell")
    (description
     "This package provides a converter plugin to the bson format for
Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-from-sqlite-0.26
  (package
    (name "rust-nu-plugin-from-sqlite")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_from_sqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0898ar0q2r0p5f8vr6w290b3wfyq2wrlygz7rhf8qfcxc3mdxxi2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rusqlite" ,rust-rusqlite-0.24)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://www.nushell.sh")
    (synopsis "Converter plugin to the bson format for Nushell")
    (description
     "This package provides a converter plugin to the bson format for
Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-inc-0.26
  (package
    (name "rust-nu-plugin-inc")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_inc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1v72zaqhzf37x3sba43c6pry75v4aacqsk4mw7fnmx6mfzbpjdn6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-test-support" ,rust-nu-test-support-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-semver" ,rust-semver-0.10))))
    (home-page "https://www.nushell.sh")
    (synopsis "Version incrementer plugin for Nushell")
    (description
     "This package provides a version incrementer plugin for
Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-match-0.26
  (package
    (name "rust-nu-plugin-match")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_match" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0iifbi739x077wwvjgdjw2h7cr149sznvjl6c5m06llmvniga4sa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-regex" ,rust-regex-1))))
    (home-page "https://www.nushell.sh")
    (synopsis "Regex match plugin for Nushell")
    (description
     "This package provides a regex match plugin for Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-post-0.26
  (package
    (name "rust-nu-plugin-post")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_post" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05frjzgrci7f9fvfqls3067ipnn2ihqxydimi7kbwajs0kzj7b45"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.12)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-surf" ,rust-surf-1)
        ("rust-url" ,rust-url-2))))
    (home-page "https://www.nushell.sh")
    (synopsis "HTTP post plugin for Nushell")
    (description "This package is an HTTP post plugin for Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-ps-0.26
  (package
    (name "rust-nu-plugin-ps")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_ps" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1znj1hjy2c2bj14hjx6yh4r3dknb7qflza1vmq5x98vnvf04i8c4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-futures-timer" ,rust-futures-timer-3)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-sysinfo" ,rust-sysinfo-0.16))))
    (home-page "https://www.nushell.sh")
    (synopsis "Process list plugin for Nushell")
    (description
     "This package provides a process list plugin for Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-s3-0.26
  (package
    (name "rust-nu-plugin-s3")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_s3" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0978rsy95qdvwnh5m2bl8ypl7rb7c0dv3zcnd986jyq3n1ykqykj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-s3handler" ,rust-s3handler-0.5))))
    (home-page "https://www.nushell.sh")
    (synopsis "S3 plugin for Nushell")
    (description "This package is an S3 plugin for Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-selector-0.26
  (package
    (name "rust-nu-plugin-selector")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_selector" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0vcwa4g6j8w27g2m4wkk04gja2bw8s9m72v7hbcdmfncgq4cnjrw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-nipper" ,rust-nipper-0.1)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26))))
    (home-page "https://www.nushell.sh")
    (synopsis "Web scraping using CSS selector")
    (description
     "This package provides web scraping using CSS selector.")
    (license license:expat)))

(define-public rust-nu-plugin-start-0.26
  (package
    (name "rust-nu-plugin-start")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_start" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0k7yqjrj7ydihcpjm8wgf3v4n8w16rawvxk9hdfysgmmvzw29kk8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-glob" ,rust-glob-0.3)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-open" ,rust-open-1)
        ("rust-url" ,rust-url-2))))
    (home-page "https://www.nushell.sh")
    (synopsis "Plugin to open files/URLs directly from Nushell")
    (description
     "This package provides a plugin to open files/URLs directly from
Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-sys-0.26
  (package
    (name "rust-nu-plugin-sys")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02jddzflzb173g8s6026vi0vk887h4p1q1a2cmc5g31r20z6h61p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-sysinfo" ,rust-sysinfo-0.16))))
    (home-page "https://www.nushell.sh")
    (synopsis "System info plugin for Nushell")
    (description "This package provides a system info plugin for Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-textview-0.26
  (package
    (name "rust-nu-plugin-textview")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_textview" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12zxphslc56n7s60i8603pq4g9iji58fg5zjvx2g6b06h83070qv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("bat" ,bat)
        ("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-nu-data" ,rust-nu-data-0.26)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-url" ,rust-url-2))))
    (home-page "https://www.nushell.sh")
    (synopsis "Text viewer plugin for Nushell")
    (description "This package provides a text viewer plugin for
Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-to-bson-0.26
  (package
    (name "rust-nu-plugin-to-bson")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_to_bson" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1vpd5j2z2jjqbf8ks6df76xr86rim1wr7x3y1m77c4v1icsnxgzs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bson" ,rust-bson-0.14)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://www.nushell.sh")
    (synopsis "Converter plugin to the bson format for Nushell")
    (description
     "This package provides a converter plugin to the bson format for
Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-to-sqlite-0.26
  (package
    (name "rust-nu-plugin-to-sqlite")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_to_sqlite" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "05z4cwaq55rfwdvr3vr8kqdyqbzpkbrdpz1mp4d9fa9b273xs393"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-hex" ,rust-hex-0.4)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rusqlite" ,rust-rusqlite-0.24)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://www.nushell.sh")
    (synopsis "Converter plugin to the bson format for Nushell")
    (description
     "This package provides a converter plugin to the bson format for
Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-tree-0.26
  (package
    (name "rust-nu-plugin-tree")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_tree" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1q80fr430igfw9a4qa5l3ydw6xhppzhqf2i6pvfjmdgf2vhflpdr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-derive-new" ,rust-derive-new-0.5)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-ptree" ,rust-ptree-0.3))))
    (home-page "https://www.nushell.sh")
    (synopsis "Tree viewer plugin for Nushell")
    (description "This package provides a tree viewer plugin for
Nushell.")
    (license license:expat)))

(define-public rust-nu-plugin-xpath-0.26
  (package
    (name "rust-nu-plugin-xpath")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu_plugin_xpath" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0sp3q5wlmwpzsx9751wqis2xxshck0fl6gpl1jckwivg8s9glbbg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-plugin" ,rust-nu-plugin-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-sxd-document" ,rust-sxd-document-0.3)
        ("rust-sxd-xpath" ,rust-sxd-xpath-0.4))))
    (home-page "https://www.nushell.sh")
    (synopsis "Traverses XML")
    (description "Traverses XML")
    (license license:expat)))

(define-public rust-nu-protocol-0.26
  (package
    (name "rust-nu-protocol")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-protocol" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1s2lm9aydgcwi0h9jhpi9ba5i55k7iv70fpgl85cm7dlw4kyzjv0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-byte-unit" ,rust-byte-unit-4)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-derive-new" ,rust-derive-new-0.5)
        ("rust-getset" ,rust-getset-0.1)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-num-integer" ,rust-num-integer-0.1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-toml" ,rust-toml-0.5))))
    (home-page "https://www.nushell.sh")
    (synopsis "Core values and protocols for Nushell")
    (description "Core values and protocols for Nushell")
    (license license:expat)))

(define-public rust-nu-source-0.26
  (package
    (name "rust-nu-source")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-source" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1sdmasal3qqcp8fjpqncppc81m0984bp528lb9zggipbxzjpvb3i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-derive-new" ,rust-derive-new-0.5)
        ("rust-getset" ,rust-getset-0.1)
        ("rust-pretty" ,rust-pretty-0.5)
        ("rust-serde" ,rust-serde-1)
        ("rust-termcolor" ,rust-termcolor-1))))
    (home-page "https://www.nushell.sh")
    (synopsis "Source string characterizer for Nushell")
    (description
     "This package provides a source string characterizer for
Nushell.")
    (license license:expat)))

(define-public rust-nu-stream-0.26
  (package
    (name "rust-nu-stream")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-stream" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1y7f28zxfrlhi9qrpbk6dpzb3wr6f59yhx3yim001796c1x6xlcs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26))))
    (home-page "https://www.nushell.sh")
    (synopsis "Nushell stream")
    (description "This package provides Nushell stream.")
    (license license:expat)))

(define-public rust-nu-table-0.26
  (package
    (name "rust-nu-table")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-table" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kxgmngfzi13xr2r65f5mxy24a8d1d0xrhi9bfjp5955r8r09km4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-regex" ,rust-regex-1)
        ("rust-unicode-width" ,rust-unicode-width-0.1))))
    (home-page "https://www.nushell.sh")
    (synopsis "Nushell table printing")
    (description "Nushell table printing")
    (license license:expat)))

(define-public rust-nu-test-support-0.26
  (package
    (name "rust-nu-test-support")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-test-support" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nlr9sqjiwviw15x76cc87n6366fxkxz1lsfh6m2ncca1zwgpa69"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bigdecimal" ,rust-bigdecimal-0.2)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-dunce" ,rust-dunce-1)
        ("rust-getset" ,rust-getset-0.1)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-indexmap" ,rust-indexmap-1)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-nu-value-ext" ,rust-nu-value-ext-0.26)
        ("rust-num-bigint" ,rust-num-bigint-0.3)
        ("rust-tempfile" ,rust-tempfile-3))))
    (home-page "https://www.nushell.sh")
    (synopsis "Support for writing Nushell tests")
    (description "This package provides support for writing Nushell
tests.")
    (license license:expat)))

(define-public rust-nu-value-ext-0.26
  (package
    (name "rust-nu-value-ext")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nu-value-ext" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12qwxhkxdxsg7q6kbsqg46127lsy150b53kpbly0s8cgwra3bhv5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-indexmap" ,rust-indexmap-1)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-nu-errors" ,rust-nu-errors-0.26)
        ("rust-nu-protocol" ,rust-nu-protocol-0.26)
        ("rust-nu-source" ,rust-nu-source-0.26)
        ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://www.nushell.sh")
    (synopsis "@code{Extension} traits for values in Nushell")
    (description
     "This package provides @code{Extension} traits for values in
Nushell.")
    (license license:expat)))
