;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2014 Kevin Lemonnier <lemonnierk@ulrar.net>
;;; Copyright © 2015 Jeff Mickey <j@codemac.net>
;;; Copyright © 2016 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 ng0 <ng0@no-reply.infotropique.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
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
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages scheme)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public dash
  (package
    (name "dash")
    (version "0.5.9.1")
    (source
     (origin
       ;; The canonical source is offline, so we fetch the source code
       ;; from the Git repository. See:
       ;; https://www.mail-archive.com/dash@vger.kernel.org/msg01323.html
       (method git-fetch)
       (uri (git-reference
              (url "https://git.kernel.org/pub/scm/utils/dash/dash.git/")
              (commit (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0p01vx7rbyf5hyyaff7h8cbhq81bm5fmq1m933484lncl9rafcai"))
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
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("libedit" ,libedit)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'bootstrap
           (lambda _ (zero? (system* "autoreconf" "-vfi")))))
       #:configure-flags '("--with-libedit")))
    (home-page "http://gondor.apana.org.au/~herbert/dash")
    (synopsis "POSIX-compliant shell optimised for size")
    (description
     "dash is a POSIX-compliant @command{/bin/sh} implementation that aims to be
as small as possible, often without sacrificing speed.  It is faster than the
GNU Bourne-Again Shell (@command{bash}) at most scripted tasks.  dash is a
direct descendant of NetBSD's Almquist Shell (@command{ash}).")
    (license (list bsd-3
                   gpl2+))))    ; mksignames.c

(define-public fish
  (package
    (name "fish")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://fishshell.com/files/"
                                  version "/fish-" version ".tar.gz"))
              (sha256
               (base32
                "1yzx73kg5ng5ivhi68756sl5hpb8869110l9fwim6gn7f7bbprby"))
              (modules '((guix build utils)))
              ;; Don't try to install /etc/fish/config.fish.
              (snippet
               '(substitute* "Makefile.in"
                  ((".*INSTALL.*sysconfdir.*fish.*") "")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)))
    (inputs
     `(("bc" ,bc)
       ("ncurses" ,ncurses)
       ("groff" ,groff)               ;for 'fish --help'
       ("pcre2" ,pcre2)               ;don't use the bundled PCRE2
       ("python" ,python-wrapper)))   ;for fish_config and manpage completions
    (arguments
     '(#:tests? #f ; no check target
       #:configure-flags '("--sysconfdir=/etc")
       #:phases
       (modify-phases %standard-phases
         ;; Embed absolute paths to store items.
         (add-after 'unpack 'embed-store-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* '("share/functions/math.fish"
                            "share/functions/seq.fish")
               (("\\| bc")
                (string-append "| " (assoc-ref %build-inputs "bc")
                               "/bin/bc")))
             (substitute* "share/functions/fish_update_completions.fish"
               (("python") (which "python")))
             (substitute* "share/functions/__fish_print_help.fish"
               (("nroff") (which "nroff")))
             #t)))))
    (synopsis "The friendly interactive shell")
    (description
     "Fish (friendly interactive shell) is a shell focused on interactive use,
discoverability, and friendliness.  Fish has very user-friendly and powerful
tab-completion, including descriptions of every completion, completion of
strings with wildcards, and many completions for specific commands.  It also
has extensive and discoverable help.  A special help command gives access to
all the fish documentation in your web browser.  Other features include smart
terminal handling based on terminfo, an easy to search history, and syntax
highlighting.")
    (home-page "https://fishshell.com/")
    (license gpl2)))

(define-public fish-guix
  (package
    (name "fish-guix")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dist.pragmatique.xyz/fish-guix/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0xi0j9lvzh43lrj82gz52n2cjln0i0pgayngrg4hy5w4449biy0z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No checks.
       #:make-flags (list
                     (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No configure script.
    (home-page "https://www.infotropique.org/projects/fish-guix/")
    (synopsis "Fish completions for Guix")
    (description
     "Fish-guix provides completions for Guix for users of the fish shell.")
    (license public-domain)))

(define-public rc
  (package
    (name "rc")
    (version "1.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://github.com/rakitzis/rc.git")
                    ;; commit name 'release: rc-1.7.4'
                    (commit "c884da53a7c885d46ace2b92de78946855b18e92")))
              (sha256
               (base32
                "00mgzvrrh9w96xa85g4gjbsvq02f08k4jwjcdnxq7kyh5xgiw95l"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--with-edit=gnu")
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'autoreconf
          (lambda _ (zero? (system* "autoreconf" "-vfi"))))
         (add-before
          'autoreconf 'patch-trip.rc
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
    (license zlib)))

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
    (license public-domain)))

(define-public tcsh
  (package
    (name "tcsh")
    (version "6.20.00")
    (source (origin
              (method url-fetch)
              ;; Old tarballs are moved to old/.
              (uri (list (string-append "ftp://ftp.astron.com/pub/tcsh/"
                                        "tcsh-" version ".tar.gz")
                         (string-append "ftp://ftp.astron.com/pub/tcsh/"
                                        "old/tcsh-" version ".tar.gz")))
              (sha256
               (base32
                "17ggxkkn5skl0v1x0j6hbv5l0sgnidfzwv16992sqkdm983fg7dq"))
              (patches (search-patches "tcsh-fix-autotest.patch"
                                       "tcsh-fix-out-of-bounds-read.patch"))
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
              (system* "make" "tests/testsuite")

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
    (home-page "http://www.tcsh.org/")
    (synopsis "Unix shell based on csh")
    (description
     "Tcsh is an enhanced, but completely compatible version of the Berkeley
UNIX C shell (csh).  It is a command language interpreter usable both as an
interactive login shell and a shell script command processor.  It includes a
command-line editor, programmable word completion, spelling correction, a
history mechanism, job control and a C-like syntax.")
    (license bsd-4)))

(define-public zsh
  (package
    (name "zsh")
    (version "5.2")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                           "http://www.zsh.org/pub/zsh-" version
                           ".tar.gz")
                         (string-append
                           "http://www.zsh.org/pub/old/zsh-" version
                           ".tar.gz")))
              (sha256
               (base32
                "0dsr450v8nydvpk8ry276fvbznlrjgddgp7zvhcw4cv69i9lr4ps"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags '("--with-tcsetpgrp" "--enable-pcre")
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
                                      (("/bin/sh") (which "sh")))))))))
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
    (home-page "http://www.zsh.org/")

    ;; The whole thing is under an MIT/X11-style license, but there's one
    ;; command, 'Completion/Unix/Command/_darcs', which is under GPLv2+.
    (license gpl2+)))

(define-public xonsh
  (package
    (name "xonsh")
    (version "0.5.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "xonsh" version))
        (sha256
          (base32
            "09s5k7fh4p0vkq0fha4ikwqlqsyv84vmlbqn8ggn0ymd47ajv38z"))
        (modules '((guix build utils)))
        (snippet
         `(begin
            ;; Delete bundled ply.
            (delete-file-recursively "xonsh/ply")
            (substitute* '("setup.py")
              (("'xonsh\\.ply\\.ply',") ""))
            #t))))
    (build-system python-build-system)
    (arguments
     '(;; TODO Try running run the test suite.
       ;; See 'requirements-tests.txt' in the source distribution for more
       ;; information.
       #:tests? #f))
    (inputs
     `(("python-ply" ,python-ply)))
    (home-page "http://xon.sh/")
    (synopsis "Python-ish shell")
    (description
     "Xonsh is a Python-ish, BASHwards-looking shell language and command
prompt.  The language is a superset of Python 3.4+ with additional shell
primitives that you are used to from Bash and IPython.  It works on all major
systems including Linux, Mac OSX, and Windows.  Xonsh is meant for the daily
use of experts and novices alike.")
    (license bsd-2)))

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
               #t))
           (add-before 'configure 'autoreconf
             (lambda _
               (zero? (system* "autoreconf")))))))
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
      (license bsd-3))))

(define-public linenoise
  (package
    (name "linenoise")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/antirez/linenoise/"
                           "archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "05006hd56xcvxjdpll4x720bpfan7vwqmxbw8a2kvm10w57ll1gm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;No tests are included
       #:make-flags (list "CC=gcc")
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
    (license bsd-2)))

(define-public s-shell
  (let ((commit "6604341edb3a775ff94415762af3ee9bd86bfb3c")
        (revision "1"))
    (package
      (name "s-shell")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rain-1/s")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1075cml6dl15d770j3m12yz90cjacsdslbv3gank1nxd76vmpdcr"))))
      (build-system gnu-build-system)
      (inputs
       `(("linenoise" ,linenoise)))
      (arguments
       `(#:tests? #f
         #:make-flags (list "CC=gcc")
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
       "S is a new shell that aims to be extremely simple.
S does not implemnt the POSIX shell standard.
There are no globs or \"splatting\" where a variable $FOO turns into multiple
command line arguments.  One token stays one token forever.
This is a \"no surprises\" straightforward approach.

There are no redirection operators > in the shell language, they are added as
extra programs.  > is just another unix command, < is essentially cat(1).
A @code{andglob} program is also provided along with s.")
      (license bsd-3))))

(define-public loksh
  (package
    (name "loksh")
    (version "6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dimkr/loksh/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wg7ds56yr8fgg1m149bi53bvrwccwiashmwknggza1sqgj9m2lq"))))
    (build-system gnu-build-system)
    (inputs
     `(("libbsd" ,libbsd)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #f ;No tests included
       #:make-flags (list "CC=gcc" "HAVE_LIBBSD=1"
                          (string-append "DESTDIR="
                                         (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ;No configure script
    (home-page "https://github.com/dimkr/loksh")
    (synopsis "Korn Shell from OpenBSD")
    (description
     "loksh is a Linux port of OpenBSD's @command{ksh}.  It is a small,
interactive POSIX shell targeted at resource-constrained systems.")
    ;; The file 'LEGAL' says it is the public domain, and the 2
    ;; exceptions which are listed are not included in this port.
    (license public-domain)))
