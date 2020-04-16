;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Alex Sassmannshausen <alex@pompo.co>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2016, 2019, 2020 Eraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017 Adonay "adfeno" Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@openmailbox.org>
;;; Copyright © 2016 Amirouche <amirouche@hypermove.net>
;;; Copyright © 2016, 2019 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017 David Thompson <davet@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018, 2019, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 swedebugia <swedebugia@riseup.net>
;;; Copyright © 2019, 2020 Amar Singh <nly@disroot.org>
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Evan Straw <evan.straw99@gmail.com>
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

(define-module (gnu packages guile-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages noweb)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages search)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

(define-public artanis
  (package
    (name "artanis")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/artanis/artanis-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0nnmdfx5xwcc3kck64var7msz7g3qk817d7bv9l159nkmic0v9w4"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Unbundle guile-redis and guile-json
                  (delete-file-recursively "artanis/third-party/json.scm")
                  (delete-file-recursively "artanis/third-party/json")
                  (delete-file-recursively "artanis/third-party/redis.scm")
                  (delete-file-recursively "artanis/third-party/redis")
                  (substitute* '("artanis/artanis.scm"
                                 "artanis/lpc.scm"
                                 "artanis/oht.scm")
                    (("(#:use-module \\()artanis third-party (json\\))" _
                      use-module json)
                     (string-append use-module json)))
                  (substitute* '("artanis/lpc.scm"
                                 "artanis/session.scm")
                    (("(#:use-module \\()artanis third-party (redis\\))" _
                      use-module redis)
                     (string-append use-module redis)))
                  (substitute* "artanis/oht.scm"
                    (("([[:punct:][:space:]]+)(->json-string)([[:punct:][:space:]]+)"
                      _ pre json-string post)
                     (string-append pre
                                    "scm" json-string
                                    post)))
                  (substitute* "artanis/artanis.scm"
                    (("[[:punct:][:space:]]+->json-string[[:punct:][:space:]]+")
                     ""))
                  #t))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-2.2)
       ("nss" ,nss)))
    ;; FIXME the bundled csv contains one more exported procedure
    ;; (sxml->csv-string) than guile-csv. The author is maintainer of both
    ;; projects.
    ;; TODO: Add guile-dbi and guile-dbd optional dependencies.
    (propagated-inputs
     `(("guile-json" ,guile-json-1)
       ("guile-readline" ,guile-readline)
       ("guile-redis" ,guile-redis)))
    (native-inputs
     `(("bash"       ,bash)         ;for the `source' builtin
       ("pkgconfig"  ,pkg-config)
       ("util-linux" ,util-linux))) ;for the `script' command
    (arguments
     '(#:make-flags
       ;; TODO: The documentation must be built with the `docs' target.
       (let* ((out (assoc-ref %outputs "out"))
              (scm (string-append out "/share/guile/site/2.2"))
              (go  (string-append out "/lib/guile/2.2/site-ccache")))
         ;; Don't use (%site-dir) for site paths.
         (list (string-append "MOD_PATH=" scm)
               (string-append "MOD_COMPILED_PATH=" go)))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-site-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "artanis/commands/help.scm"
               (("\\(%site-dir\\)")
                (string-append "\""
                               (assoc-ref outputs "out")
                               "/share/guile/site/2.2\"")))))
         (add-after 'unpack 'patch-reference-to-libnss
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "artanis/security/nss.scm"
               (("ffi-binding \"libnss3\"")
                (string-append
                 "ffi-binding \""
                 (assoc-ref inputs "nss") "/lib/nss/libnss3.so"
                 "\"")))
             #t))
         (add-before 'install 'substitute-root-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out")))
               (substitute* "Makefile"   ;ignore the execution of bash.bashrc
                 ((" /etc/bash.bashrc") " /dev/null"))
               (substitute* "Makefile"   ;set the root of config files to OUT
                 ((" /etc") (string-append " " out "/etc")))
               (mkdir-p (string-append out "/bin")) ;for the `art' executable
               #t)))
         (add-after 'install 'wrap-art
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (scm (string-append out "/share/guile/site/2.2"))
                    (go  (string-append out "/lib/guile/2.2/site-ccache")))
               (wrap-program (string-append bin "/art")
                 `("GUILE_LOAD_PATH" ":" prefix
                   (,scm ,(getenv "GUILE_LOAD_PATH")))
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                   (,go ,(getenv "GUILE_LOAD_COMPILED_PATH"))))
               #t))))))
    (synopsis "Web application framework written in Guile")
    (description "GNU Artanis is a web application framework written in Guile
Scheme.  A web application framework (WAF) is a software framework that is
designed to support the development of dynamic websites, web applications, web
services and web resources.  The framework aims to alleviate the overhead
associated with common activities performed in web development.  Artanis
provides several tools for web development: database access, templating
frameworks, session management, URL-remapping for RESTful, page caching, and
more.")
    (home-page "https://www.gnu.org/software/artanis/")
    (license (list license:gpl3+ license:lgpl3+)))) ;dual license

;; There has not been any release yet.
(define-public guildhall
  (let ((commit "2fe2cc539f4b811bbcd69e58738db03eb5a2b778")
        (revision "1"))
    (package
      (name "guildhall")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ijp/guildhall.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "115bym7bg66h3gs399yb2vkzc2ygriaqsn4zbrg8f054mgy8wzn1"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; Tests fail without this fix because they try to load the bash
           ;; executable as a Scheme file.  See bug report at
           ;; https://github.com/ijp/guildhall/issues/22
           (add-after 'unpack 'fix-bug-22
             (lambda _
               (substitute* "Makefile.am"
                 (("TESTS_ENVIRONMENT=.*")
                  "AM_TESTS_ENVIRONMENT=srcdir=$(abs_top_srcdir)/tests/
TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER= $(top_builddir)/env $(GUILE)
AM_SCM_LOG_FLAGS =  --no-auto-compile -s")
                 ;; FIXME: one of the database tests fails for unknown
                 ;; reasons.  It does not fail when run outside of Guix.
                 (("tests/database.scm") ""))
               #t)))))
      (inputs
       `(("guile" ,guile-2.0)))
      (native-inputs
       `(("zip" ,zip) ; for tests
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("texinfo" ,texinfo)))
      (synopsis "Package manager for Guile")
      (description
       "Guildhall is a package manager written for Guile Scheme.  A guild is
an association of independent craftspeople.  A guildhall is where they meet.
This Guildhall aims to make a virtual space for Guile wizards and journeyfolk
to share code.

On a practical level, Guildhall lets you share Scheme modules and programs
over the internet, and install code that has been shared by others.  Guildhall
can handle dependencies, so when a program requires several libraries, and
each of those has further dependencies, all of the prerequisites for the
program can be installed in one go.")
      (home-page "https://github.com/ijp/guildhall")
      (license license:gpl3+))))

(define-public guile-aspell
  (package
    (name "guile-aspell")
    (version "0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://lonelycactus.com/tarball/guile_aspell-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0vpk5xj9m9qc702z3khmkwhgpb949qbsyz8kw2qycda6qnxk0077"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-guilesitedir
                    (lambda _
                      (substitute* "Makefile.in"
                        (("^guilesitedir =.*$")
                         "guilesitedir = \
$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
                      #t))
                  (add-before 'build 'set-libaspell-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((aspell (assoc-ref inputs "aspell")))
                        (substitute* "aspell.scm"
                          (("\"libaspell\\.so\"")
                           (string-append "\"" aspell
                                          "/lib/libaspell\"")))
                        #t))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("guile" ,guile-2.2)
              ("aspell" ,aspell)))
    (home-page "https://github.com/spk121/guile-aspell")
    (synopsis "Spell-checking from Guile")
    (description
     "guile-aspell is a Guile Scheme library for comparing a string against a
dictionary and suggesting spelling corrections.")
    (license license:gpl3+)))

(define-public guile-bash
  ;; This project is currently retired.  It was initially announced here:
  ;; <https://lists.gnu.org/archive/html/guile-user/2015-02/msg00003.html>.
  (let ((commit "1eabc563ca5692b3e08d84f1f0e6fd2283284469")
        (revision "0"))
    (package
      (name "guile-bash")
      (version (string-append "0.1.6-" revision "." (string-take commit 7)))
      (home-page
       "https://anonscm.debian.org/cgit/users/kaction-guest/retired/dev.guile-bash.git")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (commit commit)
                      (url home-page)))
                (sha256
                 (base32
                  "097vny990wp2qpjij6a5a5gwc6fxzg5wk56inhy18iki5v6pif1p"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system gnu-build-system)
      (arguments
       '(#:configure-flags
         ;; Add -I to match 'bash.pc' of Bash 4.4.
         (list (string-append "CPPFLAGS=-I"
                              (assoc-ref %build-inputs "bash:include")
                              "/include/bash/include")

               ;; The '.a' file is useless.
               "--disable-static"

               ;; Install 'lib/bash' as Bash 4.4 expects.
               (string-append "--libdir=" (assoc-ref %outputs "out")
                              "/lib/bash"))))
      (native-inputs `(("pkg-config" ,pkg-config)
                       ("autoconf" ,autoconf-wrapper)
                       ("automake" ,automake)
                       ("libtool" ,libtool)
                       ;; Gettext brings 'AC_LIB_LINKFLAGS_FROM_LIBS'.
                       ("gettext" ,gettext-minimal)

                       ;; Bash with loadable module support, for the test
                       ;; suite.
                       ("bash-full" ,bash)))
      (inputs `(("guile" ,guile-2.0)
                ("bash:include" ,bash "include")))
      (synopsis "Extend Bash using Guile")
      (description
       "Guile-Bash provides a shared library and set of Guile modules,
allowing you to extend Bash in Scheme.  Scheme interfaces allow you to access
the following aspects of Bash:

@itemize
@item aliases;
@item setting and getting Bash variables;
@item creating dynamic variables;
@item creating Bash functions with a Scheme implementation;
@item reader macro for output capturing;
@item reader macro for evaluating raw Bash commands.
@end itemize

To enable it, run:

@example
enable -f ~/.guix-profile/lib/bash/libguile-bash.so scm
@end example

and then run @command{scm example.scm}.")
      (license license:gpl3+))))

(define-public guile-8sync
  (package
    (name "guile-8sync")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/8sync/8sync-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "031wm13srak3wsnll7j2mbbi29g1pcm4swdb71ds9yn567pn20qw"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)
                     ("guile" ,guile-2.2)
                     ("pkg-config" ,pkg-config)
                     ("texinfo" ,texinfo)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'setenv
                    (lambda _
                      ;; quiet warnings
                      (setenv "GUILE_AUTO_COMPILE" "0")
                      #t)))))
    (home-page "https://gnu.org/s/8sync/")
    (synopsis "Asynchronous actor model library for Guile")
    (description
     "GNU 8sync (pronounced \"eight-sync\") is an asynchronous programming
library for GNU Guile based on the actor model.

Note that 8sync is only available for Guile 2.2.")
    (license license:lgpl3+)))

(define-public guile-daemon
  (package
    (name "guile-daemon")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "08gaqrgjlly9k5si72vvpbr4xhq5v52l5ma5y6a7spid5dd057cy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)))
    (home-page "https://github.com/alezost/guile-daemon")
    (synopsis "Evaluate code in a running Guile process")
    (description
     "Guile-Daemon is a small Guile program that loads your initial
configuration file, and then reads and evaluates Guile expressions that
you send to a FIFO file.")
    (license license:gpl3+)))

(define-public guile-dsv
  (package
    (name "guile-dsv")
    (version "0.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/artyom-poptsov/guile-dsv")
                    (commit "bdc5267d007478abc20ea96d7c459b7dd9560b3d")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1irw6mz8998nwyhzrw9g94jcz60b9zljgqfmipaz1ybn8579qjx0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
    (inputs `(("guile" ,guile-2.2)))
    (propagated-inputs `(("guile-lib" ,guile-lib)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-guilesitedir
                    (lambda _
                      (substitute* "Makefile.in"
                        (("^guilesitedir =.*$")
                         "guilesitedir = \
$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
                      (substitute* "modules/Makefile.in"
                        (("^guilesitedir =.*$")
                         "guilesitedir = \
$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
                      (substitute* "modules/dsv/Makefile.in"
                        (("^guilesitedir =.*$")
                         "guilesitedir = \
$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
                      #t)))))
    (home-page "https://github.com/artyom-poptsov/guile-dsv")
    (synopsis "DSV module for Guile")
    (description
     "Guile-DSV is a GNU Guile module for working with the
delimiter-separated values (DSV) data format.  Guile-DSV supports the
Unix-style DSV format and RFC 4180 format.")
    (license license:gpl3+)))

(define-public guile-fibers
  (package
    (name "guile-fibers")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wingolog.org/pub/fibers/fibers-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0vjkg72ghgdgphzbjz9ig8al8271rq8974viknb2r1rg4lz92ld0"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure"
                    (("search=\"2\\.2\"")
                     "search=\"3.0 2.2\""))

                  ;; Explicitly include system headers rather than relying on
                  ;; <libguile.h> to do it for us.
                  (substitute* "epoll.c"
                    (("#include.*libguile\\.h.*$" all)
                     (string-append "#include <unistd.h>\n"
                                    "#include <string.h>\n"
                                    all "\n")))

                  ;; Import (ice-9 threads) for 'current-processor-count'.
                  (substitute* "tests/channels.scm"
                    (("#:use-module \\(fibers\\)")
                     (string-append "#:use-module (fibers)\n"
                                    "#:use-module (ice-9 threads)\n")))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'install 'mode-guile-objects
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; .go files are installed to "lib/guile/X.Y/cache".
                      ;; This phase moves them to "…/site-ccache".
                      (let* ((out (assoc-ref outputs "out"))
                             (lib (string-append out "/lib/guile"))
                             (old (car (find-files lib "^ccache$"
                                                   #:directories? #t)))
                             (new (string-append (dirname old)
                                                 "/site-ccache")))
                        (rename-file old new)
                        #t))))))
    (native-inputs
     `(("texinfo" ,texinfo)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)))
    (synopsis "Lightweight concurrency facility for Guile")
    (description
     "Fibers is a Guile library that implements a a lightweight concurrency
facility, inspired by systems like Concurrent ML, Go, and Erlang.  A fiber is
like a \"goroutine\" from the Go language: a lightweight thread-like
abstraction.  Systems built with Fibers can scale up to millions of concurrent
fibers, tens of thousands of concurrent socket connections, and many parallel
cores.  The Fibers library also provides Concurrent ML-like channels for
communication between fibers.

Note that Fibers makes use of some Guile 2.1/2.2-specific features and
is not available for Guile 2.0.")
    (home-page "https://github.com/wingo/fibers")
    (license license:lgpl3+)))

(define-public guile3.0-fibers
  (package
    (inherit guile-fibers)
    (name "guile3.0-fibers")
    (arguments
     ;; The code uses 'scm_t_uint64' et al., which are deprecated in 3.0.
     `(#:configure-flags '("CFLAGS=-Wno-error=deprecated-declarations")
       ,@(package-arguments guile-fibers)))
    (inputs `(("guile" ,guile-3.0)))))

(define-public guile-syntax-highlight
  (package
    (name "guile-syntax-highlight")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/"
                                  "guile-syntax-highlight/"
                                  "guile-syntax-highlight-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1p771kq15x83483m23bhah1sz6vkalg3drm7x279f4j1cxligkzi"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure"
                    (("2\\.2 2\\.0")
                     "3.0 2.2 2.0"))
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)))
    (synopsis "General-purpose syntax highlighter for GNU Guile")
    (description "Guile-syntax-highlight is a general-purpose syntax
highlighting library for GNU Guile.  It can parse code written in various
programming languages into a simple s-expression that can be converted to
HTML (via SXML) or any other format for rendering.")
    (home-page "https://dthompson.us/projects/guile-syntax-highlight.html")
    (license license:lgpl3+)))

(define-public guile3.0-syntax-highlight
  (package
    (inherit guile-syntax-highlight)
    (name "guile3.0-syntax-highlight")
    (inputs `(("guile" ,guile-3.0)))))

(define-public guile-sjson
  (package
    (name "guile-sjson")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dustycloud.org/misc/sjson-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1mzmapln79vv10qxaggz9qwcdbag3jnrj19xx8bgkmxss8h03sv3"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)))
    (home-page "https://gitlab.com/dustyweb/guile-sjson")
    (synopsis "S-expression based json reader/writer for Guile")
    (description "guile-sjson is a json reader/writer for Guile.
It has a nice, simple s-expression based syntax.")
    (license license:lgpl3+)))

(define-public guile-squee
  (let ((commit "a85902a92bf6f58a1d35fd974a01ade163deda8d")
        (revision "0"))
    (package
      (name "guile-squee")
      (version (string-append "0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://notabug.org/cwebber/guile-squee.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0p1lpsp4kx57j3ai1dkxilm4ziavzzx8wbbc42m3hpziq0a7qz5z"))))
      (build-system guile-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "squee.scm"
                 (("dynamic-link \"libpq\"")
                  (string-append
                   "dynamic-link \""
                   (assoc-ref inputs "postgresql") "/lib/libpq.so"
                   "\"")))
               #t)))))
      (inputs
       `(("postgresql" ,postgresql)))
      (native-inputs
       `(("guile" ,guile-2.2)))
      (home-page "https://notabug.org/cwebber/guile-squee")
      (synopsis "Connect to PostgreSQL using Guile")
      (description
       "@code{squee} is a Guile library for connecting to PostgreSQL databases
using Guile's foreign function interface.")
      (license license:lgpl3+))))

(define-public guile3.0-squee
  (package
    (inherit guile-squee)
    (name "guile3.0-squee")
    (native-inputs `(("guile" ,guile-next)
                     ,@(alist-delete "guile"
                                     (package-native-inputs guile-squee))))))

(define-public guile-colorized
  (package
    (name "guile-colorized")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/NalaGinrut/guile-colorized.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10mv8c63159r3qvwwdvsgnsvdg7nc2ghak85zapwqpv4ywrqp9zc"))))
    (build-system guile-build-system)
    (native-inputs
     `(("guile" ,guile-2.2)))
    (home-page "https://gitlab.com/NalaGinrut/guile-colorized")
    (synopsis "Colorized REPL for Guile")
    (description
     "Guile-colorized provides you with a colorized REPL for GNU Guile.")
    (license license:gpl3+)))

(define-public guile3.0-colorized
  (package
    (inherit guile-colorized)
    (name "guile3.0-colorized")
    (native-inputs `(("guile" ,guile-next)))))

(define-public guile-pfds
  (package
    (name "guile-pfds")
    (version "0.3")
    (home-page "https://github.com/ijp/pfds")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "19y33wg94pf0n98dkfqd1zbw93fgky4sawxsxl6s3vyqwl0yi5vh"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system guile-build-system)
    (arguments
     '(#:source-directory "src"
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'move-files-around
                    (lambda _
                      ;; Move files under a pfds/ directory to reflect the
                      ;; module hierarchy.
                      (mkdir-p "src/pfds")
                      (for-each (lambda (file)
                                  (rename-file file
                                               (string-append "src/pfds/"
                                                              file)))
                                '("bbtrees.sls"
                                  "deques"
                                  "deques.sls"
                                  "dlists.sls"
                                  "fingertrees.sls"
                                  "hamts.sls"
                                  "heaps.sls"
                                  "private"
                                  "psqs.sls"
                                  "queues"
                                  "queues.sls"
                                  "sequences.sls"
                                  "sets.sls"))

                      ;; In Guile <= 2.2.4, there's no way to tell 'guild
                      ;; compile' to accept the ".sls" extension.  So...
                      (for-each (lambda (file)
                                  (rename-file file
                                               (string-append
                                                (string-drop-right file 4)
                                                ".scm")))
                                (find-files "." "\\.sls$"))
                      #t)))))
    (native-inputs
     `(("guile" ,guile-2.2)))
    (synopsis "Purely functional data structures for Guile")
    (description
     "This package provides purely functional data structures written in R6RS
Scheme and compiled for Guile.  It has been tested with Racket, Guile 2,
Vicare Scheme and IronScheme.  Right now it contains:

@itemize
@item queues
@item deques
@item bbtrees
@item sets
@item dlists
@item priority search queues (PSQs)
@item finger trees
@item sequences
@item heaps
@item hash array mapped tries (HAMTs).
@end itemize\n")
    (license license:bsd-3)))

(define-public guile3.0-pfds
  (package
    (inherit guile-pfds)
    (name "guile3.0-pfds")
    (native-inputs `(("guile" ,guile-3.0)))
    (arguments
     (substitute-keyword-arguments (package-arguments guile-pfds)
       ((#:phases phases)
        `(modify-phases ,phases
          (add-after 'unpack 'work-around-guile-bug
            (lambda _
              ;; See bug #39210.
              (substitute* '("fingertrees.sls"
                             "queues/private/condition.sls"
                             "deques/private/condition.sls")
                (("&assertion") "&violation"))
              #t))))))))

(define-public guile-aa-tree
  (package
    (name "guile-aa-tree")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-aa-tree/guile-aa-tree-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0044c105r3q9vpl17pv3phl1b79kjm1llhkakqgiasixyav01blh"))))
    (build-system guile-build-system)
    (native-inputs `(("guile" ,guile-2.2)))
    ;; https://savannah.nongnu.org/projects/guile-aa-tree
    (home-page "https://qlfiles.net/guile-aa-tree/")
    (synopsis "AA tree data structure for Guile")
    (description
     "This package provides an implementation of @dfn{AA trees}, a
self-balancing binary tree data structure, for Guile.  It ensure @math{O(log
n)} worst case performance for core operations.  The module provides
non-mutating insert, delete, and search operations, with support for
convenient nested tree operations.")
    (license license:gpl3+)))

(define-public guile-simple-zmq
  (let ((commit "68bedb6679716214fb9d3472da57544526f7a618")
        (revision "3"))
    (package
      (name "guile-simple-zmq")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jerry40/guile-simple-zmq")
               (commit commit)))
         (sha256
          (base32
           "1ad3xg69qqviy1f6dnlw0ysmfdbmp1jq65rfqb8nfd8dsrq2syli"))
         (file-name (git-file-name name version))))
      (build-system guile-build-system)
      (arguments
       `(#:source-directory "src"
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'set-libzmq-file-name
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "src/simple-zmq.scm"
                          (("\\(dynamic-link \"libzmq\"\\)")
                           (format #f "(dynamic-link \"~a/lib/libzmq.so\")"
                                   (assoc-ref inputs "zeromq"))))
                        #t)))))
      (native-inputs
       `(("guile" ,guile-2.2)))
      (inputs
       `(("zeromq" ,zeromq)))
      (home-page "https://github.com/jerry40/guile-simple-zmq")
      (synopsis "Guile wrapper over ZeroMQ library")
      (description
       "This package provides a Guile programming interface to the ZeroMQ
messaging library.")
      (license license:gpl3+))))

(define-public guile3.0-simple-zmq
  (package
    (inherit guile-simple-zmq)
    (name "guile3.0-simple-zmq")
    (native-inputs `(("guile" ,guile-next)))))

(define-public jupyter-guile-kernel
  (let ((commit "a7db9245a886e104138474df46c3e88b95cff629")
        (revision "1"))
    (package
      (name "jupyter-guile-kernel")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jerry40/guile-kernel")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0aj04853bqm47ivfcmrgpb7w3wkis847kc7qrwsa5zcn9h38qh2f"))))
      (build-system guile-build-system)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'set-openssl-file-name
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; Record the absolute file name of the 'openssl'
                        ;; command.
                        (substitute* "src/hmac.scm"
                          (("openssl")
                           (string-append (assoc-ref inputs "openssl")
                                          "/bin/openssl")))
                        #t))

                    ;; XXX: The code uses 'include' to include its own source
                    ;; files, and "-L src" isn't enough in this case.
                    (add-before 'build 'chdir
                      (lambda _ (chdir "src") #t))
                    (add-after 'build 'chdir-back
                      (lambda _ (chdir "..") #t))

                    (add-after 'install 'install-kernel
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out   (assoc-ref outputs "out"))
                               (json  (assoc-ref inputs "guile-json"))
                               (zmq   (assoc-ref inputs "guile-simple-zmq"))
                               (deps  (list json zmq))
                               (dir   (string-append
                                       out "/share/jupyter/kernels/guile"))
                               (effective (target-guile-effective-version)))
                          ;; Install kernel.
                          (install-file "src/kernel.json" dir)

                          ;; Fix hard-coded file name in the kernel.
                          (substitute* (string-append dir "/kernel.json")
                            (("/home/.*/guile-jupyter-kernel.scm")
                             (string-append out "/share/guile/site/"
                                            (target-guile-effective-version)
                                            "/guile-jupyter-kernel.scm"))
                            (("\"guile\"")
                             (string-append "\"" (assoc-ref inputs "guile")
                                            "/bin/guile\""))
                            (("-s")
                             ;; Add '-L' and '-C' flags so that the kernel
                             ;; finds its dependencies.
                             (let ((-L (map (lambda (item)
                                              (string-append "\"" item
                                                             "/share/guile/site/"
                                                             effective "\""))
                                            deps))
                                   (-C (map (lambda (item)
                                              (string-append "\"" item
                                                             "/lib/guile/"
                                                             effective
                                                             "/site-ccache\""))
                                            deps)))
                              (string-append "--no-auto-compile\""
                                             (string-join -L ", \"-L\", "
                                                          'prefix)
                                             (string-join -C ", \"-C\", "
                                                          'prefix)
                                             ", \"-s"))))
                          #t))))))
      (inputs
       `(("openssl" ,openssl)
         ("guile" ,guile-2.2)
         ("guile-json" ,guile-json-1)
         ("guile-simple-zmq" ,guile-simple-zmq)))
      (synopsis "Guile kernel for the Jupyter Notebook")
      (description
       "This package provides a Guile 2.x kernel for the Jupyter Notebook.  It
allows users to interact with the Guile REPL through Jupyter.")
      (home-page "https://github.com/jerry40/guile-kernel")
      (license license:gpl3+))))

(define-public guile-sparql
  (package
   (name "guile-sparql")
   (version "0.0.7")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/roelj/guile-sparql/releases/download/"
                  version "/guile-sparql-" version ".tar.gz"))
            (sha256
             (base32 "1drnvhsgl0gc5crmb16yyw1j98nkhwwcgssv9vgm36ng43nnzffd"))))
   (build-system gnu-build-system)
   (arguments `(#:tests? #f)) ; There are no tests.
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (inputs
    `(("guile" ,guile-2.2)))
   (home-page "https://github.com/roelj/guile-sparql")
   (synopsis "SPARQL module for Guile")
   (description "This package provides the functionality to query a SPARQL
endpoint.  Additionally, it provides an interface to write SPARQL queries
using S-expressions.")
   (license license:gpl3+)))

(define-public guile-debbugs
  (package
    (name "guile-debbugs")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/guile-debbugs/guile-debbugs-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1cc63nw3xdfjrfk8c58r6d5lidmfq5cpqcy32yd5xp81yccprvn9"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("guile-email" ,guile-email)))
    (native-inputs
     `(("guile" ,guile-2.2)
       ("pkg-config" ,pkg-config)))
    (home-page "https://savannah.gnu.org/projects/guile-debbugs/")
    (synopsis "Guile interface to the Debbugs bug tracking service")
    (description
     "This package provides a Guile library to communicate with a Debbugs bug
tracker's SOAP service, such as @url{https://bugs.gnu.org}.")
    (license license:gpl3+)))

(define-public guile-email
  (package
    (name "guile-email")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://guile-email.systemreboot.net/releases/guile-email-"
             version ".tar.lz"))
       (sha256
        (base32
         "1rc8r0fgvflnyq5ckl7ii8sghpsgpkzxa8vskjr1ak2kyar6m35k"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("lzip" ,lzip)))
    (inputs
     `(("guile" ,guile-2.2)))
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0"))) ; to prevent guild warnings
    (home-page "https://guile-email.systemreboot.net")
    (synopsis "Guile email parser")
    (description "guile-email is a collection of email utilities implemented
in pure guile.  It supports parsing MIME (Multipurpose Internet Mail
Extensions) compliant email messages and reading emails from the mbox
format.")
    (license license:agpl3+)))

(define-public guile3.0-email
  (package
    (inherit guile-email)
    (name "guile3.0-email")
    (inputs `(("guile" ,guile-next)
              ,@(alist-delete "guile" (package-inputs guile-email))))))

(define-public guile-newt
  (package
    (name "guile-newt")
    (version "0.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/mothacehe/guile-newt")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gksd1lzgjjh1p9vczghg8jw995d22hm34kbsiv8rcryirv2xy09"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0"))) ;to prevent guild warnings
    (inputs
     `(("guile" ,guile-2.2)
       ("newt" ,newt)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (synopsis "Guile bindings to Newt")
    (description
     "This package provides bindings for Newt, a programming library for
color text mode, widget based user interfaces.  The bindings are written in pure
Scheme by using Guile’s foreign function interface.")
    (home-page "https://gitlab.com/mothacehe/guile-newt")
    (license license:gpl3+)))

(define-public guile3.0-newt
  (package
    (inherit guile-newt)
    (name "guile3.0-newt")
    (inputs `(("guile" ,guile-next)
              ,@(alist-delete "guile" (package-inputs guile-newt))))))

(define-public guile-mastodon
  (package
    (name "guile-mastodon")
    (version "0.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://framagit.org/prouby/guile-mastodon.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vblf3d1bbwna3l09p2ap5y8ycvl549bz6whgk78imyfmn28ygry"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)
       ("gnutls" ,gnutls)
       ("guile-json" ,guile-json-1)))
    (home-page "https://framagit.org/prouby/guile-mastodon")
    (synopsis "Guile Mastodon REST API module")
    (description "This package provides Guile modules to access the
@uref{https://docs.joinmastodon.org/api/, REST API of Mastodon}, a federated
microblogging service.")
    (license license:gpl3+)))

(define-public guile-parted
  (package
    (name "guile-parted")
    (version "0.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/mothacehe/guile-parted")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b7h8psfm9gmmwb65pp5zwzglvwnfmw5j40g09hhf3f7kwxc0mv2"))
              (modules '((guix build utils)))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0"))) ;to prevent guild warnings
    (inputs
     `(("guile" ,guile-2.2)
       ("parted" ,parted)))
    (propagated-inputs
     `(("guile-bytestructures" ,guile-bytestructures)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (synopsis "Guile bindings to GNU Parted")
    (description
     "This package provides bindings for GNU Parted library, a C library
allowing disk partition tables creation and manipulation.  The bindings are
written in pure Scheme by using Guile's foreign function interface.")
    (home-page "https://gitlab.com/mothacehe/guile-parted")
    (license license:gpl3+)))

(define-public guile3.0-parted
  (package
    (inherit guile-parted)
    (name "guile3.0-parted")
    (inputs `(("guile" ,guile-next)
              ,@(alist-delete "guile" (package-inputs guile-parted))))
    (propagated-inputs
     `(("guile-bytestructures" ,guile3.0-bytestructures)))))

(define-public guile-xosd
  (package
    (name "guile-xosd")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ri5065c16kmgrf2pysn2ymxjqi5302lhpb07wkl1jr75ym8fn8p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxinerama" ,libxinerama)
       ("xosd" ,xosd)))
    (home-page "https://github.com/alezost/guile-xosd")
    (synopsis "XOSD bindings for Guile")
    (description
     "Guile-XOSD provides Guile bindings for @code{libxosd},
@uref{http://sourceforge.net/projects/libxosd/, the X On Screen Display
library}.")
    (license license:gpl3+)))

(define-public guile-dbi
  (package
    (name "guile-dbi")
    (version "2.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.gna.org/guile-dbi/guile-dbi-"
                    version ".tar.gz"))
              (sha256
               (base32
                "116njrprhgrsv1qm904sp3b02rq01fx639r433d657gyhw3x159n"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append
              "--with-guile-site-dir=" %output "/share/guile/site/2.2"))
       #:make-flags
       (list (string-append
              "LDFLAGS=-Wl,-rpath=" %output "/lib:"
              (assoc-ref %build-inputs "guile-dbd-sqlite3") "/lib" ":"
              (assoc-ref %build-inputs "guile-dbd-postgresql") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-extension-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (dbi.scm (string-append
                              out "/share/guile/site/2.2/dbi/dbi.scm"))
                    (ext     (string-append out "/lib/libguile-dbi")))
               (substitute* dbi.scm (("libguile-dbi") ext))
               #t))))))
    (inputs
     `(("guile-dbd-sqlite3" ,guile-dbd-sqlite3)
       ("guile-dbd-postgresql" ,guile-dbd-postgresql))) ; only shared library, no scheme files
    (propagated-inputs
     `(("guile" ,guile-2.2)))
    (synopsis "Guile database abstraction layer")
    (home-page "http://home.gna.org/guile-dbi/guile-dbi.html")
    (description
     "guile-dbi is a library for Guile that provides a convenient interface to
SQL databases.  Database programming with guile-dbi is generic in that the same
programming interface is presented regardless of which database system is used.
It currently supports MySQL, Postgres and SQLite3.")
    (license license:gpl2+)))

(define guile-dbi-bootstrap
  (package
    (inherit guile-dbi)
    (name "guile-dbi-bootstrap")
    (inputs '())
    (arguments
     (substitute-keyword-arguments (package-arguments guile-dbi)
       ((#:make-flags _) '(list))))))

(define-public guile-dbd-sqlite3
  (package
    (name "guile-dbd-sqlite3")
    (version "2.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.gna.org/guile-dbi/guile-dbd-sqlite3-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0rg71jchxd2y8x496s8zmfmikr5g8zxi8zv2ar3f7a23pph92iw2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("guile-dbi-bootstrap" ,guile-dbi-bootstrap))) ; only required for headers
    (inputs
     `(("sqlite" ,sqlite)
       ("zlib" ,(@ (gnu packages compression) zlib))))
    (synopsis "Guile DBI driver for SQLite")
    (home-page "https://github.com/jkalbhenn/guile-dbd-sqlite3")
    (description
     "guile-dbi is a library for Guile that provides a convenient interface to
SQL databases.  This package implements the interface for SQLite.")
    (license license:gpl2+)))

(define-public guile-dbd-postgresql
  (let ((commit "e97589b6b018b206c901e4cc24db463407a4036b")
        (revision 0))
    (package
      (name "guile-dbd-postgresql")
      (version (string-append
                "2.1.6-" (number->string revision) "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/opencog/guile-dbi.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0n1gv9a0kdys10a4qmnrwvg5sydwb03880asri4gqdchcj3fimni"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               ;; The upstream Git repository contains all the code, so change
               ;; to the relevant directory.
               (chdir "guile-dbd-postgresql")
               #t))
           (add-after 'chdir 'patch-src/Makefile.am
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/Makefile.am"
                 (("/usr/include")
                  (string-append (assoc-ref inputs "postgresql") "/include")))
               #t))
           (add-after 'patch-src/Makefile.am 'patch-src
             (lambda _
               (substitute* "src/guile-dbd-postgresql.c"
                 (("postgresql/libpq-fe\\.h") "libpq-fe.h"))
               #t)))))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("automake" ,automake)
         ("autoconf" ,autoconf)
         ("perl" ,perl)
         ("libtool" ,libtool)
         ("guile-dbi-bootstrap" ,guile-dbi-bootstrap)))
      (inputs
       `(("postgresql" ,postgresql)
         ("zlib" ,zlib)))
      (synopsis "Guile DBI driver for PostgreSQL")
      (home-page
       "https://github.com/opencog/guile-dbi/tree/master/guile-dbd-postgresql")
      (description
       "@code{guile-dbi} is a library for Guile that provides a convenient
interface to SQL databases.  This package implements the interface for
PostgreSQL.")
      (license license:gpl2+))))

(define-public guile-config
  (package
    (name "guile-config")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/a-sassmannshausen/guile-config")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 (base32
                "0gglsqwpw77gvrqcny8irpqfl7qdf2v8n9ggwrswanxalj4vcbvf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
    (inputs `(("guile" ,guile-2.2)))
    (synopsis
     "Guile application configuration parsing library.")
    (description
     "Guile Config is a library providing a declarative approach to
application configuration specification.  The library provides clean
configuration declaration forms, and processors that take care of:
configuration file creation; configuration file parsing; command-line
parameter parsing using getopt-long; basic GNU command-line parameter
generation (--help, --usage, --version); automatic output generation for the
above command-line parameters.")
    (home-page
     "https://gitlab.com/a-sassmannshausen/guile-config")
    (license license:gpl3+)))

(define-public guile3.0-config
  (package
    (inherit guile-config)
    (name "guile3.0-config")
    (inputs `(("guile" ,guile-next)
              ,@(alist-delete "guile" (package-inputs guile-config))))))

(define-public guile-hall
  (package
    (name "guile-hall")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/a-sassmannshausen/guile-hall")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 (base32
                "0dwx5iyg0dmdf64wq0b4w306bapr86jwnw35npgbjq4cqm8qbzqn"))))
    (build-system gnu-build-system)
    (arguments
      `(#:modules
        ((ice-9 match)
         (ice-9 ftw)
         ,@%gnu-build-system-modules)
        #:phases
        (modify-phases
          %standard-phases
          (add-after 'install 'hall-wrap-binaries
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((compiled-dir
                       (lambda (out version)
                         (string-append
                           out "/lib/guile/" version "/site-ccache")))
                     (uncompiled-dir
                       (lambda (out version)
                         (string-append
                          out "/share/guile/site"
                          (if (string-null? version) "" "/") version)))
                     (dep-path
                       (lambda (env modules path)
                         (list env ":" 'prefix
                               (cons modules
                                     (map (lambda (input)
                                            (string-append
                                              (assoc-ref inputs input)
                                              path))
                                          ,''("guile-config"))))))
                     (out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin/"))
                     (site (uncompiled-dir out "")))
                (match (scandir site)
                       (("." ".." version)
                        (for-each
                          (lambda (file)
                            (wrap-program
                              (string-append bin file)
                              (dep-path
                                "GUILE_LOAD_PATH"
                                (uncompiled-dir out version)
                                (uncompiled-dir "" version))
                              (dep-path
                                "GUILE_LOAD_COMPILED_PATH"
                                (compiled-dir out version)
                                (compiled-dir "" version))))
                          ,''("hall"))
                        #t))))))))
    (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("pkg-config" ,pkg-config)
        ("texinfo" ,texinfo)))
    (inputs `(("guile" ,guile-2.2)))
    (propagated-inputs
     `(("guile-config" ,guile-config)))
    (synopsis "Guile project tooling")
    (description
     "Hall is a command-line application and a set of Guile libraries that
allow you to quickly create and publish Guile projects.  It allows you to
transparently support the GNU build system, manage a project hierarchy &
provides tight coupling to Guix.")
    (home-page "https://gitlab.com/a-sassmannshausen/guile-hall")
    (license license:gpl3+)))

(define-public guile3.0-hall
  (package
    (inherit guile-hall)
    (name "guile3.0-hall")
    (inputs `(("guile" ,guile-next)
              ,@(alist-delete "guile" (package-inputs guile-hall))))
    (propagated-inputs
     `(("guile-config" ,guile3.0-config)
       ,@(alist-delete "guile-config"
                       (package-propagated-inputs guile-hall))))))

(define-public guile-ics
  (package
    (name "guile-ics")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/artyom-poptsov/guile-ics")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0qjjvadr7gibdq9jvwkmlkb4afsw9n2shfj9phpiadinxk3p4m2g"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure.ac"
                    (("^GUILE_PKG.*")
                     "GUILE_PKG([3.0 2.2 2.0])\n"))
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf-wrapper)
       ("automake" ,automake)
       ("texinfo" ,texinfo)
       ;; Gettext brings 'AC_LIB_LINKFLAGS_FROM_LIBS'.
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs `(("guile" ,guile-2.2) ("which" ,which)))
    (propagated-inputs `(("guile-lib" ,guile-lib)))
    (home-page "https://github.com/artyom-poptsov/guile-ics")
    (synopsis "Guile parser library for the iCalendar format")
    (description
     "Guile-ICS is an iCalendar (RFC5545) format parser library written in
pure Scheme.  The library can be used to read and write iCalendar data.

The library is shipped with documentation in Info format and usage examples.")
    (license license:gpl3+)))

(define-public guile3.0-ics
  (package
    (inherit guile-ics)
    (name "guile3.0-ics")
    (inputs `(("guile" ,guile-3.0)
              ,@(alist-delete "guile" (package-inputs guile-ics))))
    (propagated-inputs `(("guile-lib" ,guile3.0-lib)))))

(define-public guile-wisp
  (package
    (name "guile-wisp")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/ArneBab/"
                                  "wisp/downloads/wisp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03pz7pj9jyallynhflp5s7qax8dj1fs8la434wrfgz7g1kgjnvf6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build emacs-build-system) #:prefix emacs:)
                  (guix build utils)
                  (guix build emacs-utils)
                  (ice-9 rdelim)
                  (ice-9 popen))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-build-system)
                           (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-/usr/bin/env
           (lambda _
             (substitute* "Makefile.in"
               (("/usr/bin/env bash") (which "bash")))
             #t))
         ;; auto compilation breaks, but if we set HOME to /tmp,
         ;; that works ok
         (add-before 'check 'auto-compile-hacky-workaround
           (lambda _ (setenv "HOME" "/tmp") #t))
         (add-after 'install 'install-go-files
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (effective (read-line
                                (open-pipe* OPEN_READ
                                            "guile" "-c"
                                            "(display (effective-version))")))
                    (module-dir (string-append out "/share/guile/site/"
                                               effective))
                    (object-dir (string-append out "/lib/guile/" effective
                                               "/site-ccache"))
                    (prefix     (string-length module-dir)))
               ;; compile to the destination
               (for-each (lambda (file)
                           (let* ((base (string-drop (string-drop-right file 4)
                                                     prefix))
                                  (go   (string-append object-dir base ".go")))
                             (invoke "guild" "compile" "-L" module-dir
                                     file "-o" go)))
                         (find-files module-dir "\\.scm$"))
               #t)))
         (add-after 'install 'install-emacs-files
           (assoc-ref emacs:%standard-phases 'install))
         (add-after 'install-emacs-files 'compile-emacs-files
           (assoc-ref emacs:%standard-phases 'build))
         (add-after 'compile-emacs-files 'make-autoloads
           (assoc-ref emacs:%standard-phases 'make-autoloads)))))
    (home-page "https://www.draketo.de/english/wisp")
    (inputs
     `(("guile" ,guile-2.2)))
    (native-inputs
     `(("emacs" ,emacs-minimal)
       ("python" ,python)
       ("pkg-config" ,pkg-config)))
    (synopsis "Whitespace to lisp syntax for Guile")
    (description "Wisp is a syntax for Guile which provides a Python-like
whitespace-significant language.  It may be easier on the eyes for some
users and in some situations.")
    (license license:gpl3+)))

(define-public guile3.0-wisp
  (package
    (inherit guile-wisp)
    (name "guile3.0-wisp")
    (inputs `(("guile" ,guile-3.0)))
    (arguments
     (substitute-keyword-arguments (package-arguments guile-wisp)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'support-guile-3.0
             (lambda _
               (substitute* "configure"
                 (("_guile_versions_to_search=\"2.2")
                  "_guile_versions_to_search=\"3.0 2.2"))
               #t))))))))

(define-public guile-sly
  (package
    (name "guile-sly")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/sly/sly-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1svzlbz2vripmyq2kjh0rig16bsrnbkwbsm558pjln9l65mcl4qq"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "configure"
                    (("_guile_required_version=\"2.0.11\"")
                     "_guile_required_version=\"2\"")
                    (("ac_subst_vars='")
                     "ac_subst_vars='GUILE_EFFECTIVE_VERSION\n"))
                  (substitute* (find-files "." "Makefile.in")
                    (("moddir = .*$")
                     (string-append
                      "moddir = "
                      "$(prefix)/share/guile/site/@GUILE_EFFECTIVE_VERSION@\n"))
                    (("godir = .*$")
                     (string-append
                      "godir = "
                      "$(prefix)/lib/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache\n")))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-libfreeimage-prefix="
                            (assoc-ref %build-inputs "freeimage"))
             (string-append "--with-libgslcblas-prefix="
                            (assoc-ref %build-inputs "gsl")))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("guile-sdl" ,guile-sdl)
       ("guile-opengl" ,guile-opengl)))
    (inputs
     `(("guile" ,guile-2.2)
       ("gsl" ,gsl)
       ("freeimage" ,freeimage)
       ("mesa" ,mesa)))
    (synopsis "2D/3D game engine for GNU Guile")
    (description "Sly is a 2D/3D game engine written in Guile Scheme.  Sly
features a functional reactive programming interface and live coding
capabilities.")
    (home-page "https://dthompson.us/projects/sly.html")
    (license license:gpl3+)))

(define-public g-golf
  (let ((commit "4a4edf25e4877df9182c77843bdd98ab59e13ef7"))
    (package
      (name "g-golf")
      (version (git-version "1" "683" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/g-golf.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09p0gf71wbmlm9kri693a8fvr9hl3hhlmlidyadwjdh7853xg0h8"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("texinfo" ,texinfo)
         ("gettext" ,gettext-minimal)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("guile" ,guile-2.2)
         ("guile-lib" ,guile-lib)
         ("clutter" ,clutter)
         ("gtk" ,gtk+)
         ("glib" ,glib)))
      (propagated-inputs
       `(("gobject-introspection" ,gobject-introspection)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'configure 'tests-work-arounds
             (lambda* (#:key inputs #:allow-other-keys)
               ;; In build environment, There is no /dev/tty
               (substitute*
                   "test-suite/tests/gobject.scm"
                 (("/dev/tty") "/dev/null"))))
           (add-before 'configure 'substitute-libs
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((get (lambda (key lib)
                             (string-append (assoc-ref inputs key) "/lib/" lib)))
                      (libgi      (get "gobject-introspection" "libgirepository-1.0"))
                      (libglib    (get "glib" "libglib-2.0"))
                      (libgobject (get "glib" "libgobject-2.0"))
                      (libgdk     (get "gtk" "libgdk-3")))
                 (substitute* "configure"
                   (("SITEDIR=\"\\$datadir/g-golf\"")
                    "SITEDIR=\"$datadir/guile/site/$GUILE_EFFECTIVE_VERSION\"")
                   (("SITECCACHEDIR=\"\\$libdir/g-golf/")
                    "SITECCACHEDIR=\"$libdir/"))
                 (substitute* "g-golf/init.scm"
                   (("libgirepository-1.0") libgi)
                   (("libglib-2.0") libglib)
                   (("libgdk-3") libgdk)
                   (("libgobject-2.0") libgobject)
                   (("\\(dynamic-link \"libg-golf\"\\)")
                    (format #f "~s"
                            `(dynamic-link
                              (format #f "~alibg-golf"
                                      (if (getenv "GUILE_GGOLF_UNINSTALLED")
                                          ""
                                          ,(format #f "~a/lib/"
                                                   (assoc-ref outputs "out"))))))))
                 (setenv "GUILE_AUTO_COMPILE" "0")
                 (setenv "GUILE_GGOLF_UNINSTALLED" "1")
                 #t))))))
      (home-page "https://www.gnu.org/software/g-golf/")
      (synopsis "Guile bindings for GObject Introspection")
      (description
       "G-Golf (Gnome: (Guile Object Library for)) is a library for developing
modern applications in Guile Scheme.  It comprises a direct binding to the
GObject Introspection API and higher-level functionality for importing Gnome
libraries and making GObject classes (and methods) available in Guile's
object-oriented programming system, GOOPS.")
      (license license:lgpl3+))))

(define-public g-wrap
  (package
    (name "g-wrap")
    (version "1.9.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/g-wrap/g-wrap-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ak0bha37dfpj9kmyw1r8fj8nva639aw5xr66wr5gd3l1rqf5xhg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("guile" ,guile-2.2)
       ("guile-lib" ,guile-lib)))
    (inputs
     `(("libffi" ,libffi)))
    (arguments
     `(#:configure-flags '("--disable-Werror")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (find-files "." "^Makefile.in$")
                 (("guilemoduledir =.*guile/site" all)
                  (string-append all "/@GUILE_EFFECTIVE_VERSION@")))
               #t))))))
    (synopsis "Generate C bindings for Guile")
    (description "G-Wrap is a tool and Guile library for generating function
wrappers for inter-language calls.  It currently only supports generating Guile
wrappers for C functions.  Given a definition of the types and prototypes for
a given C interface, G-Wrap will automatically generate the C code that
provides access to that interface and its types from the Scheme level.")
    (home-page "https://www.nongnu.org/g-wrap/index.html")
    (license license:lgpl2.1+)))

(define-public guile-miniadapton
  (let ((commit "1b5749422304567c96ac5367f2221dda9eff5880")
        (revision "1"))
    (package
      (name "guile-miniadapton")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/fisherdj/miniAdapton.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "09q51zkw2fypad5xixskfzw2cjhjgs5cswdp3i7cpp651rb3zndh"))))
      (build-system guile-build-system)
      (native-inputs
       `(("guile" ,guile-2.2)))
      (home-page "https://github.com/fisherdj/miniAdapton")
      (synopsis "Minimal implementation of incremental computation in Guile
Scheme")
      (description "This package provides a complete Scheme implementation of
miniAdapton, which implements the core functionality of the Adapton system for
incremental computation (also known as self-adjusting computation).  Like
Adapton, miniAdapton allows programmers to safely combine mutation and
memoization.  miniAdapton is built on top of an even simpler system,
microAdapton.  Both miniAdapton and microAdapton are designed to be easy to
understand, extend, and port to host languages other than Scheme.")
      (license license:expat))))

(define-public guile-reader
  (package
    (name "guile-reader")
    (version "0.6.2")
    (source  (origin
               (method url-fetch)
               (uri (string-append "mirror://savannah/guile-reader/guile-reader-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "0592s2s8ampqmqwilc4fvcild6rb9gy79di6vxv5kcdmv23abkgx"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkgconfig" ,pkg-config)
                     ("gperf" ,gperf-3.0)))
    (inputs `(("guile" ,guile-2.2)))
    (synopsis "Framework for building readers for GNU Guile")
    (description
     "Guile-Reader is a simple framework for building readers for GNU Guile.

The idea is to make it easy to build procedures that extend Guile’s read
procedure.  Readers supporting various syntax variants can easily be written,
possibly by re-using existing “token readers” of a standard Scheme
readers.  For example, it is used to implement Skribilo’s R5RS-derived
document syntax.

Guile-Reader’s approach is similar to Common Lisp’s “read table”, but
hopefully more powerful and flexible (for instance, one may instantiate as
many readers as needed).")
    (home-page "https://www.nongnu.org/guile-reader/")
    (license license:gpl3+)))

(define-public guile-ncurses
  (package
    (name "guile-ncurses")
    (version "3.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/guile-ncurses/guile-ncurses-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "038xbffalhymg26lvmzgf7ljilxz2f2zmqg5r5nfzbipfbprwjhf"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("guile" ,guile-2.2)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:configure-flags (list "--with-ncursesw"  ; Unicode support
                               "--with-gnu-filesystem-hierarchy")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-libguile-ncurses-file-name
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "install"
                     "-C" "src/ncurses"
                     "-j" (number->string
                           (parallel-job-count)))
             (let* ((out   (assoc-ref outputs "out"))
                    (dir   "src/ncurses")
                    (files (find-files dir ".scm")))
               (substitute* files
                 (("\"libguile-ncurses\"")
                  (format #f "\"~a/lib/guile/2.2/libguile-ncurses\""
                          out)))
               #t))))))
    (home-page "https://www.gnu.org/software/guile-ncurses/")
    (synopsis "Guile bindings to ncurses")
    (description
     "guile-ncurses provides Guile language bindings for the ncurses
library.")
    (license license:lgpl3+)))

(define-public guile-ncurses/gpm
  (package
    (inherit guile-ncurses)
    (name "guile-ncurses-with-gpm")
    (inputs `(("ncurses" ,ncurses/gpm)
              ("guile" ,guile-2.2)))))

(define-public guile-lib
  (package
    (name "guile-lib")
    (version "0.2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-lib/guile-lib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0aizxdif5dpch9cvs8zz5g8ds5s4xhfnwza2il5ji7fv2h7ks7bd"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Work around miscompilation on Guile 3.0.0 at -O2:
                  ;; <https://bugs.gnu.org/39251>.
                  (substitute* "src/md5.scm"
                    (("\\(define f-ash ash\\)")
                     "(define f-ash (@ (guile) ash))\n")
                    (("\\(define f-add \\+\\)")
                     "(define f-add (@ (guile) +))\n"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0")        ; to prevent guild errors
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-module-dir
           (lambda _
             (substitute* "src/Makefile.in"
               (("^moddir = ([[:graph:]]+)")
                "moddir = $(datadir)/guile/site/@GUILE_EFFECTIVE_VERSION@\n")
               (("^godir = ([[:graph:]]+)")
                "godir = \
$(libdir)/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache\n"))
             #t)))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("guile" ,guile-2.2)))
    (home-page "https://www.nongnu.org/guile-lib/")
    (synopsis "Collection of useful Guile Scheme modules")
    (description
     "Guile-Lib is intended as an accumulation place for pure-scheme Guile
modules, allowing for people to cooperate integrating their generic Guile
modules into a coherent library.  Think \"a down-scaled, limited-scope CPAN
for Guile\".")

    ;; The whole is under GPLv3+, but some modules are under laxer
    ;; distribution terms such as LGPL and public domain.  See `COPYING' for
    ;; details.
    (license license:gpl3+)))

(define-public guile2.0-lib
  (package
    (inherit guile-lib)
    (name "guile2.0-lib")
    (inputs `(("guile" ,guile-2.0)))))

(define-public guile3.0-lib
  (package
    (inherit guile-lib)
    (name "guile3.0-lib")
    (inputs `(("guile" ,guile-3.0)))))

(define-public guile-minikanren
  (package
    (name "guile-minikanren")
    (version "20150424.e844d85")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ijp/minikanren.git")
                    (commit "e844d85512f8c055d3f96143ee506007389a25e3")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0r50jlpzi940jlmxyy3ddqqwmj5r12gb4bcv0ssini9v8km13xz6"))))
    (build-system guile-build-system)
    (native-inputs
     `(("guile" ,guile-2.2)))
    (home-page "https://github.com/ijp/minikanren")
    (synopsis "MiniKanren declarative logic system, packaged for Guile")
    (description
     "MiniKanren is a relational programming extension to the Scheme
programming Language, written as a smaller version of Kanren suitable for
pedagogical purposes.  It is featured in the book, The Reasoned Schemer,
written by Dan Friedman, William Byrd, and Oleg Kiselyov.

This is Ian Price's r6rs packaged version of miniKanren, which deviates
slightly from miniKanren mainline.

See http://minikanren.org/ for more on miniKanren generally.")
    (license license:expat)))

(define-public guile2.0-minikanren
  (package
    (inherit guile-minikanren)
    (name "guile2.0-minikanren")
    (native-inputs `(("guile" ,guile-2.0)))))

(define-public guile3.0-minikanren
  (package
    (inherit guile-minikanren)
    (name "guile3.0-minikanren")
    (native-inputs `(("guile" ,guile-next)))))

(define-public guile-irregex
  (package
    (name "guile-irregex")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://synthcode.com/scheme/irregex/irregex-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1ia3m7dp3lcxa048q0gqbiwwsyvn99baw6xkhb4bhhzn4k7bwyqq"))))
    (build-system guile-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'move-files-around
                    (lambda _
                      ;; Move the relevant source files to src/ and create the
                      ;; rx/ directory to match the expected module hierarchy.
                      (mkdir-p "src/rx/source")
                      (rename-file "irregex-guile.scm"
                                   "src/rx/irregex.scm")
                      (rename-file "irregex.scm"
                                   "src/rx/source/irregex.scm")
                      ;; Not really reachable via guile's packaging system,
                      ;; but nice to have around.
                      (rename-file "irregex-utils.scm"
                                   "src/rx/source/irregex-utils.scm")
                      #t)))
       #:source-directory "src"))
    (native-inputs
     `(("guile" ,guile-2.2)))
    (home-page "http://synthcode.com/scheme/irregex")
    (synopsis "S-expression based regular expressions")
    (description
     "Irregex is an s-expression based alternative to your classic
string-based regular expressions.  It implements SRFI 115 and is deeply
inspired by the SCSH regular expression system.")
    (license license:bsd-3)))

(define-public guile2.0-irregex
  (package
    (inherit guile-irregex)
    (name "guile2.0-irregex")
    (native-inputs `(("guile" ,guile-2.0)))))

(define-public guile3.0-irregex
  (package
    (inherit guile-irregex)
    (name "guile3.0-irregex")
    (native-inputs `(("guile" ,guile-next)))))

(define-public haunt
  (package
    (name "haunt")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/haunt/haunt-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "056z4znikk83nr5mr0x2ac3iinqbywa2bvb37mhr566a1q50isfc"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure"
                    (("2\\.2 2\\.0")
                     "3.0 2.2 2.0"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((ice-9 match) (ice-9 ftw)
                  ,@%gnu-build-system-modules)
       #:tests? #f ; test suite is non-deterministic :(
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-haunt
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Wrap the 'haunt' command to refer to the right
                      ;; modules.
                      (let* ((out  (assoc-ref outputs "out"))
                             (bin  (string-append out "/bin"))
                             (site (string-append
                                    out "/share/guile/site"))
                             (guile-reader (assoc-ref inputs "guile-reader"))
                             (deps `(,@(if guile-reader
                                           (list guile-reader)
                                           '())
                                     ,(assoc-ref inputs "guile-commonmark"))))
                        (match (scandir site)
                          (("." ".." version)
                           (let ((modules (string-append site "/" version))
                                 (compiled-modules (string-append
                                                    out "/lib/guile/" version
                                                    "/site-ccache")))
                             (wrap-program (string-append bin "/haunt")
                               `("GUILE_LOAD_PATH" ":" prefix
                                 (,modules
                                  ,@(map (lambda (dep)
                                           (string-append dep
                                                          "/share/guile/site/"
                                                          version))
                                         deps)))
                               `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                                 (,compiled-modules
                                  ,@(map (lambda (dep)
                                           (string-append dep "/lib/guile/"
                                                          version
                                                          "/site-ccache"))
                                         deps))))
                             #t)))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
    (inputs
     `(("guile" ,guile-2.2)))
    (propagated-inputs
     `(("guile-reader" ,guile-reader)
       ("guile-commonmark" ,guile-commonmark)))
    (synopsis "Functional static site generator")
    (description "Haunt is a static site generator written in Guile
Scheme.  Haunt features a functional build system and an extensible
interface for reading articles in any format.")
    (home-page "http://haunt.dthompson.us")
    (license license:gpl3+)))

(define-public guile3.0-haunt
  (package
    (inherit haunt)
    (name "guile3.0-haunt")
    (inputs `(("guile" ,guile-3.0)))
    (propagated-inputs
     ;; XXX: Guile-Reader is currently unavailable for Guile 3.0 so strip it.
     `(("guile-commonmark" ,guile3.0-commonmark)))))

(define-public guile2.0-haunt
  (package
    (inherit haunt)
    (name "guile2.0-haunt")
    (inputs `(("guile" ,guile-2.0)))))

(define-public guile-redis
  (package
    (name "guile-redis")
    (version "1.3.0")
    (home-page "https://github.com/aconchillo/guile-redis")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url home-page)
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14izs8daxh7pb7vwpxi5g427qa31137jkaxrb1cy5rpjkwchy723"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("guile" ,guile-2.2)))
    (synopsis "Redis client library for Guile")
    (description "Guile-redis provides a Scheme interface to the Redis
key-value cache and store.")
    (license license:lgpl3+)))

(define-public guile2.0-redis
  (package
    (inherit guile-redis)
    (name "guile2.0-redis")
    (native-inputs `(("guile" ,guile-2.0)
                     ,@(alist-delete "guile"
                                     (package-native-inputs guile-redis))))))

(define-public guile-commonmark
  (package
    (name "guile-commonmark")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/OrangeShark/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "17lrsdisa3kckh24q114vfmzdc4wkqa6ccwl4hdlrng5wpn1iman"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure"
                    (("2\\.2 2\\.0")
                     "3.0 2.2 2.0"))
                  #t))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-2.2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "CommonMark parser for Guile")
    (description
     "guile-commonmark is a library for parsing CommonMark, a fully specified
variant of Markdown.  The library is written in Guile Scheme and is designed
to transform a CommonMark document to SXML.  guile-commonmark tries to closely
follow the @uref{http://commonmark.org/, CommonMark spec}, the main difference
is no support for parsing block and inline level HTML.")
    (home-page "https://github.com/OrangeShark/guile-commonmark")
    (license license:lgpl3+)))

(define-public guile3.0-commonmark
  (package
    (inherit guile-commonmark)
    (name "guile3.0-commonmark")
    (inputs `(("guile" ,guile-3.0)))))

(define-public guile2.0-commonmark
  (package
    (inherit guile-commonmark)
    (name "guile2.0-commonmark")
    (inputs `(("guile" ,guile-2.0)))))

(define-public mcron
  (package
    (name "mcron")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/mcron/mcron-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1521w3h33bhdlg6qc66sq4dwv3qsx8r8x6srq4ca6kaahy6dszw8"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-finding-guile
                    (lambda _
                      (substitute* "configure"
                        (("2\\.0") "3.0 2.2 2.0"))
                      #t))
                  (add-before 'check 'adjust-tests
                    (lambda _
                      (substitute* "tests/job-specifier.scm"
                        ;; (getpw) fails with "entry not found" in the build
                        ;; environment, so pass an argument.
                        (("\\(getpw\\)")
                         "(getpwnam (getuid))")
                        ;; The build environment lacks an entry for root in
                        ;; /etc/passwd.
                        (("\\(getpw 0\\)")
                         "(getpwnam \"nobody\")")

                        ;; FIXME: Skip the 4 faulty tests (see above).
                        (("\\(test-equal \"next-year\"" all)
                         (string-append "(test-skip 4)\n" all)))
                      #t)))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("tzdata" ,tzdata-for-tests)))
    (inputs `(("guile" ,guile-2.2.7)))       ;fix <https://bugs.gnu.org/37237>
    (home-page "https://www.gnu.org/software/mcron/")
    (synopsis "Run jobs at scheduled times")
    (description
     "GNU Mcron is a complete replacement for Vixie cron.  It is used to run
tasks on a schedule, such as every hour or every Monday.  Mcron is written in
Guile, so its configuration can be written in Scheme; the original cron
format is also supported.")
    (license license:gpl3+)))

(define-public guile3.0-mcron
  (package
    (inherit mcron)
    (name "guile3.0-mcron")
    (inputs `(("guile" ,guile-3.0)))))

(define-public guile-picture-language
  (let ((commit "91d10c96708d732145006dd2802acc4de08b632e")
        (revision "1"))
    (package
      (name "guile-picture-language")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.elephly.net/software/guile-picture-language.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ydvw9dvssdvlvhh1dr8inyzy2x6m41qgp8hsivca1xysr4gc23a"))))
      (build-system gnu-build-system)
      (inputs
       `(("guile" ,guile-2.2)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("imagemagick" ,imagemagick)
         ("pkg-config" ,pkg-config)
         ("texinfo" ,texinfo)))
      (home-page "https://git.elephly.net/software/guile-picture-language.git")
      (synopsis "Picture language for Guile")
      (description
       "This package provides a simple SVG-based picture language for Guile.
The picture values can directly be displayed in Geiser.")
      (license license:lgpl3+))))

(define-public guile3.0-picture-language
  (package
    (inherit guile-picture-language)
    (name "guile3.0-picture-language")
    (inputs `(("guile" ,guile-3.0)))))

(define-public guile-studio
  (package
    (name "guile-studio")
    (version "0.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.elephly.net/software/guile-studio.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10ls4ra148hd9ra7sin4kh9vv0am5pwk48p7dsjhrlg2l1hsh4hi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((ice-9 match)
        (srfi srfi-1)
        ,@%gnu-build-system-modules)
       #:tests? #f                    ; there are none
       #:make-flags
       (list (string-append "ICONS_DIR="
                            (assoc-ref %build-inputs "adwaita-icon-theme")
                            "/share/icons/Adwaita/")
             (string-append "PICT_DIR="
                            (assoc-ref %build-inputs "guile-picture-language"))
             (string-append "EMACS_DIR="
                            (assoc-ref %build-inputs "emacs"))
             (string-append "GUILE_DIR="
                            (assoc-ref %build-inputs "guile"))
             (string-join (cons "INPUTS="
                                (filter-map
                                 (lambda (input)
                                   (match input
                                     ((label . pkg)
                                      (and (string-prefix? "emacs" label) pkg))))
                                 %build-inputs)))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'install))))
    (inputs
     `(("guile" ,guile-2.2)
       ("guile-picture-language" ,guile-picture-language)
       ("emacs" ,emacs)
       ("emacs-geiser" ,emacs-geiser)
       ("emacs-company" ,emacs-company)
       ("emacs-flycheck" ,emacs-flycheck)
       ("emacs-smart-mode-line" ,emacs-smart-mode-line)
       ("emacs-paren-face" ,emacs-paren-face)
       ("adwaita-icon-theme" ,adwaita-icon-theme)))
    (native-inputs
     `(("texinfo" ,texinfo)))
    (home-page "https://gnu.org/software/guile")
    (synopsis "IDE for Guile")
    (description
     "This is Emacs with a few settings that make working with Guile easier
for people new to Emacs.  Features include: CUA mode, Geiser, tool bar icons
to evaluate Guile buffers, support for Guile's very own picture language, code
completion, a simple mode line, etc.")
    (license license:gpl3+)))

(define-public guile-stis-parser
  (let ((commit "6e85d37ffc333b722f4413a6c648263701eb75bd")
        (revision "1"))
    (package
      (name "guile-stis-parser")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/tampe/stis-parser")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0v4hvq7rlpbra1ni73lf8k6sdmjlflr50yi3p1f24g85h77pc7c0"))))
      (build-system gnu-build-system)
      (arguments
       `(#:parallel-build? #f ; not supported
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "modules") #t))
           (add-after 'chdir 'use-canonical-directory-for-go-files
             (lambda _
               (substitute* "Makefile.am"
                 (("/ccache") "/site-ccache"))
               #t))
           (add-after 'chdir 'delete-broken-symlink
             (lambda _
               (delete-file "parser/stis-parser/lang/.#calc.scm")
               #t)))))
      (inputs
       `(("guile" ,guile-2.2)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)))
      (home-page "https://gitlab.com/tampe/stis-parser")
      (synopsis "Parser combinator framework")
      (description
       "This package provides a functional parser combinator library that
supports backtracking and a small logical framework. The idea is to build up
chunks that are memoized and there is no clear scanner/parser separation,
chunks can be expressions as well as simple tokens.")
      (license license:lgpl2.0+))))

(define-public guile-persist
  (let ((commit "b14927b0368af51c024560aee5f55724aee35233")
        (revision "1"))
    (package
      (name "guile-persist")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/tampe/guile-persist")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0z5nf377wh8yj6n3sx2ddn4bdx1qrqnw899dlqjhg0q69qzil522"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Install .go files in the right place.
                    (substitute* "Makefile.am"
                      (("/ccache") "/site-ccache"))
                    #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-prefix
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "src/Makefile.am"
                 (("/usr/local/lib/guile")
                  (string-append (assoc-ref outputs "out") "/lib/guile"))
                 (("/usr/local/include/guile")
                  (string-append (assoc-ref inputs "guile") "/include/guile"))
                 (("-L/usr/local/lib")
                  (string-append "-L" (assoc-ref inputs "guile") "/lib"))
                 ;; Use canonical directory for go files.
                 (("/ccache") "/site-ccache"))
               #t))
           (add-after 'unpack 'patch-library-reference
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "persist/persistance.scm"
                   (("\"libguile-persist\"")
                    (format #f "\"~a/lib/guile/2.2/extensions/libguile-persist\"" out)))
                 #t))))))
      (inputs
       `(("guile" ,guile-2.2)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://gitlab.com/tampe/guile-persist")
      (synopsis "Persistence programming framework for Guile")
      (description
       "This is a serialization library for serializing objects like classes
and objects, closures and structs.  This currently does not support
serializing continuations or delimited continuations.")
      (license license:lgpl2.0+))))

(define-public python-on-guile
  (let ((commit "00a51a23247f1edc4ae8eda72b30df5cd7d0015f")
        (revision "3"))
    (package
      (name "python-on-guile")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.elephly.net/software/python-on-guile.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03rpnqr08rqr3gay128g564rwk8w4jbj28ss6b46z1d4vjs4nk68"))))
      (build-system gnu-build-system)
      (arguments
       `(#:parallel-build? #f ; not supported
         #:make-flags '("GUILE_AUTO_COMPILE=0")   ;to prevent guild warnings

         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "modules") #t))
           (add-after 'install 'wrap
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Wrap the 'python' executable so it can find its
               ;; dependencies.
               (let ((out  (assoc-ref outputs "out")))
                 (wrap-program (string-append out "/bin/python")
                   `("GUILE_LOAD_PATH" ":" prefix
                     (,(getenv "GUILE_LOAD_PATH")))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                     (,(getenv "GUILE_LOAD_COMPILED_PATH"))))
                 #t))))))
      (inputs
       `(("guile" ,guile-2.2)))
      (propagated-inputs
       `(("guile-persist" ,guile-persist)
         ("guile-readline" ,guile-readline)
         ("guile-stis-parser" ,guile-stis-parser)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (home-page "https://gitlab.com/python-on-guile/python-on-guile/")
      (synopsis "Python implementation in Guile")
      (description
       "This package allows you to compile a Guile Python file to any target
from @code{tree-il}.")
      (license license:lgpl2.0+))))

(define-public guile-file-names
  (package
    (name "guile-file-names")
    (version "0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://brandon.invergo.net/software/download/"
                                  "guile-file-names/guile-file-names-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "01chizdxkhw6aqv629vxka9f5x3534ij7r0jqndawsg2vxm1r9sz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-target-directory
           (lambda _
             (substitute* "src/Makefile.in"
               (("guilemoddir = \\$\\(GUILE_SITE\\)")
                "guilemoddir = $(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
             #t)))))
    (inputs
     `(("guile" ,guile-2.2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://gitlab.com/brandoninvergo/guile-file-names")
    (synopsis "Manipulate file names")
    (description
     "The @code{(file-names)} module provides tools for manipulating file
names.  The module was built on the idea that doing anything more than a
non-trivial modification of a file name string is a pain (making sure all
slashes are present and accounted for, resolving @code{.} and @code{..}, etc).
Inevitably, you have to break the string up into chunks and operate on that
list of components.  This module takes care of that for you.")
    (license license:lgpl3+)))

(define-public guile-gi
  (package
    (name "guile-gi")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://lonelycactus.com/tarball/guile_gi-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1v82kz8mz7wgq6w5llaz8a2wwdnl8vk2667dpjwjxscl0qyxsy6y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-gnu-filesystem-hierarchy")
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-references-to-extension
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((effective (read-line
                               (open-pipe* OPEN_READ
                                           "guile" "-c"
                                           "(display (effective-version))"))))
               (substitute* '("module/gi.scm"
                              "module/gi/oop.scm"
                              "module/gi/documentation.scm"
                              "module/gi/types.scm"
                              "module/gi/repository.scm")
                 (("\\(load-extension \"libguile-gi\" \"(.*)\"\\)" m arg)
                  (format #f "~s"
                          `(load-extension
                            (format #f "~alibguile-gi"
                                    (if (getenv "GUILE_GI_UNINSTALLED")
                                        ""
                                        ,(format #f "~a/lib/guile/~a/"
                                                 (assoc-ref outputs "out")
                                                 effective)))
                            ,arg)))))
             (setenv "GUILE_GI_UNINSTALLED" "1")
             #t))
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The init_check test requires a running X server.
             (system (format #f "~a/bin/Xvfb :1 &"
                             (assoc-ref inputs "xorg-server")))
             (setenv "DISPLAY" ":1")
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ; for glib-compile-resources
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server)))
    (propagated-inputs
     `(("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("gssettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("guile-lib" ,guile-lib)
       ("webkitgtk" ,webkitgtk)))
    (inputs `(("guile" ,guile-2.2)))
    (home-page "https://github.com/spk121/guile-gi")
    (synopsis "GObject bindings for Guile")
    (description
     "Guile-GI is a library for Guile that allows using GObject-based
libraries, such as GTK+3.  Its README comes with the disclaimer: This is
pre-alpha code.")
    (license license:gpl3+)))

(define-public guile3.0-gi
  (package
    (inherit guile-gi)
    (name "guile3.0-gi")
    (native-inputs
     `(("guile" ,guile-3.0)
       ,@(package-native-inputs guile-gi)))))

(define-public guile-srfi-159
  (let ((commit "1bd98abda2ae4ef8f36761a167903e55c6bda7bb")
        (revision "0"))
    (package
      (name "guile-srfi-159")
      (version (git-version "0" revision commit))
      (home-page "https://bitbucket.org/bjoli/guile-srfi-159")
      (source (origin
                (method hg-fetch)
                (uri (hg-reference (changeset commit)
                                   (url home-page)))
                (sha256
                 (base32
                  "1zw6cmcy7xdbfiz3nz9arqnn7l2daidaps6ixkcrc9b6k51fdv3p"))
                (file-name (git-file-name name version))))
      (build-system guile-build-system)
      (arguments
       ;; The *-impl.scm files are actually included from module files; they
       ;; should not be compiled separately, but they must be installed.
       '(#:not-compiled-file-regexp "-impl\\.scm$"))
      (inputs
       `(("guile" ,guile-2.2)))
      (synopsis "Formatting combinators for Guile")
      (description
       "The @code{(srfi-159)} module and its sub-modules implement the
formatting combinators specified by
@uref{https://srfi.schemers.org/srfi-159/srfi-159.html, SRFI-159}.  These are
more expressive and flexible than the traditional @code{format} procedure.")
      (license license:bsd-3))))

(define-public emacsy
  (package
    (name "emacsy")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.savannah.nongnu.org/releases/"
                    name "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cpb85dl1nibd34c2x2h7vfmjpkgh353p5b1w20v6cs6gmvgg4np"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bzip2" ,bzip2)
       ("guile" ,guile-2.2)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)
       ("texlive" ,(texlive-union (list texlive-generic-epsf)))))
    (inputs
     `(("dbus-glib" ,dbus-glib)
       ("guile" ,guile-2.2)
       ("guile-lib" ,guile-lib)
       ("guile-readline" ,guile-readline)
       ("freeglut" ,freeglut)
       ("webkitgtk" ,webkitgtk)))
    (propagated-inputs
     `(("glib-networking" ,glib-networking)
       ("gssettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (ice-9 regex)
                  (ice-9 ftw)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setenv
           (lambda _
             (setenv "GUILE_AUTO_COMPILE" "0")
             #t))
         (add-after 'install 'wrap-binaries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (effective (read-line
                                (open-pipe* OPEN_READ
                                            "guile" "-c"
                                            "(display (effective-version))")))
                    (deps (map (cut assoc-ref inputs <>)
                               '("guile-lib" "guile-readline")))
                    (scm-path (map (cut string-append <> "/share/guile/site/"
                                        effective) `(,out ,@deps)))
                    (go-path (map (cut string-append <> "/lib/guile/" effective
                                       "/site-ccache/") `(,out ,@deps)))
                    (examples (filter (cut string-match "emacsy" <>)
                                      (scandir (string-append out "/bin/"))))
                    (progs (map (cut string-append out "/bin/" <>)
                                examples)))
               (map (cut wrap-program <>
                         `("GUILE_LOAD_PATH" ":" prefix ,scm-path)
                         `("GUILE_LOAD_COMPILED_PATH" ":" prefix ,go-path))
                    progs)
               #t))))))
    (home-page "https://savannah.nongnu.org/projects/emacsy")
    (synopsis "Embeddable GNU Emacs-like library using Guile")
    (description
     "Emacsy is an embeddable Emacs-like library that uses GNU Guile
as extension language.  Emacsy can give a C program an Emacsy feel with
keymaps, minibuffer, recordable macros, history, tab completion, major
and minor modes, etc., and can also be used as a pure Guile library.  It
comes with a simple counter example using FreeGLUT and browser examples
in C using Gtk+-3 and WebKitGtk.")
    (license license:gpl3+)))

(define-public emacsy-minimal
  (let ((commit "d459ca1d3d09e7624e662bc4cfc3596850796fc6"))
    (package
      (inherit emacsy)
      (name "emacsy-minimal")
      (version (git-version "v0.4.1" "28" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/emacsy.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ps15w8cxj9kc18gmvys9jv9xa1qqa7m43ismv34l3cmhddrn0sr"))))
      (build-system gnu-build-system)
      (inputs
       `(("guile" ,guile-2.2)
         ("guile-lib" ,guile-lib)
         ("guile-readline" ,guile-readline)))
      (propagated-inputs '())
      (arguments
       `(#:configure-flags '("--without-examples")
         #:phases
         (modify-phases %standard-phases
         (add-before 'configure 'setenv
           (lambda _
             (setenv "GUILE_AUTO_COMPILE" "0")
             #t))))))))

(define-public guile-jpeg
  (let ((commit "6a1673578b297c2c1b28e44a76bd5c49e76a5046")
        (revision "0"))
    (package
      (name "guile-jpeg")
      (version (git-version "0.0" revision commit))
      (home-page "https://gitlab.com/wingo/guile-jpeg")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)))
                (sha256
                 (base32
                  "05z9m408w3h6aqb5k3r3qa7khir0k10rxwvsrzhkcq1hr5vbmr4m"))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Install .go files in the right place.
                    (substitute* "Makefile.am"
                      (("/ccache") "/site-ccache"))
                    #t))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)
         ("guile" ,guile-2.2)))
      (synopsis "JPEG file parsing library for Guile")
      (description
       "Guile-JPEG is a Scheme library to parse JPEG image files and to
perform geometrical transforms on JPEG images.")
      (license license:gpl3+))))

(define-public nomad
  (package
    (name "nomad")
    (version "0.1.2-alpha")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.gnu.org/git/nomad.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dnkr1hmvfkwgxd75dcf93pg39yfgawvdpzdhv991yhghv0qxc9h"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bash" ,bash)
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)
       ("guile" ,guile-2.2)
       ("glib:bin" ,glib "bin")
       ("texinfo" ,texinfo)
       ("perl" ,perl)))
    (inputs
     `(("guile" ,guile-2.2)
       ("guile-lib" ,guile-lib)
       ("guile-gcrypt" ,guile-gcrypt)
       ("guile-readline" ,guile-readline)
       ("gnutls" ,gnutls)
       ("shroud" ,shroud)
       ("emacsy" ,emacsy-minimal)
       ("glib" ,glib)
       ("dbus-glib" ,dbus-glib)
       ("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("webkitgtk" ,webkitgtk)
       ("xorg-server" ,xorg-server)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system (format #f "~a/bin/Xvfb :1 &"
                             (assoc-ref inputs "xorg-server")))
             (setenv "DISPLAY" ":1")
             #t))
         (add-after 'install 'wrap-binaries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gio-deps (map (cut assoc-ref inputs <>) '("glib-networking"
                                                               "glib")))
                    (gio-mod-path (map (cut string-append <> "/lib/gio/modules")
                                       gio-deps))
                    (effective (read-line (open-pipe*
                                           OPEN_READ
                                           "guile" "-c"
                                           "(display (effective-version))")))
                    (deps (map (cut assoc-ref inputs <>)
                               '("emacsy" "guile-lib" "guile-readline"
                                 "shroud")))
                    (scm-path (map (cut string-append <>
                                        "/share/guile/site/" effective)
                                   `(,out ,@deps)))
                    (go-path (map (cut string-append <>
                                       "/lib/guile/" effective "/site-ccache")
                                  `(,out ,@deps)))
                    (progs (map (cut string-append out "/bin/" <>)
                                '("nomad"))))
               (map (cut wrap-program <>
                         `("GIO_EXTRA_MODULES" ":" prefix ,gio-mod-path)
                         `("GUILE_LOAD_PATH" ":" prefix ,scm-path)
                         `("GUILE_LOAD_COMPILED_PATH" ":"
                           prefix ,go-path))
                    progs)
               #t))))))
    (home-page "https://savannah.nongnu.org/projects/nomad/")
    (synopsis "Extensible Web Browser in Guile Scheme")
    (description "Nomad is an Emacs-like Web Browser built using Webkitgtk and
Emacsy.  It has a small C layer and most browser features are fully
programmable in Guile.  It has hooks, keymaps, and self documentation
features.")
    (license license:gpl3+)))

(define-public guile-cv
  (package
    (name "guile-cv")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/guile-cv/guile-cv-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qdf0s2h1xj5lbhnc1pfw69i3zg08pqy2y6869b92ydfis8r82j9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "configure"
               (("SITEDIR=\"\\$datadir/guile-cv\"")
                "SITEDIR=\"$datadir/guile/site/$GUILE_EFFECTIVE_VERSION\"")
               (("SITECCACHEDIR=\"\\$libdir/guile-cv/")
                "SITECCACHEDIR=\"$libdir/"))
             (substitute* "cv/init.scm"
               (("\\(dynamic-link \"libvigra_c\"\\)")
                (string-append "(dynamic-link \""
                               (assoc-ref inputs "vigra-c")
                               "/lib/libvigra_c\")"))
               (("\\(dynamic-link \"libguile-cv\"\\)")
                (format #f "~s"
                        `(dynamic-link
                          (format #f "~alibguile-cv"
                                  (if (getenv "GUILE_CV_UNINSTALLED")
                                      ""
                                      ,(format #f "~a/lib/"
                                               (assoc-ref outputs "out"))))))))
             (setenv "GUILE_CV_UNINSTALLED" "1")
             ;; Only needed to satisfy the configure script.
             (setenv "LD_LIBRARY_PATH"
                     (string-append (assoc-ref inputs "vigra-c") "/lib"))
             #t)))))
    (inputs
     `(("vigra" ,vigra)
       ("vigra-c" ,vigra-c)
       ("guile" ,guile-2.2)))
    (native-inputs
     `(("texlive" ,(texlive-union (list texlive-booktabs
                                        texlive-lm
                                        texlive-siunitx
                                        texlive-standalone
                                        texlive-xcolor
                                        texlive-fonts-iwona)))
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("guile-lib" ,guile-lib)))
    (home-page "https://www.gnu.org/software/guile-cv/")
    (synopsis "Computer vision library for Guile")
    (description "Guile-CV is a Computer Vision functional programming library
for the Guile Scheme language.  It is based on Vigra (Vision with Generic
Algorithms), a C++ image processing and analysis library.  Guile-CV contains
bindings to Vigra C (a C wrapper to most of the Vigra functionality) and is
enriched with pure Guile Scheme algorithms, all accessible through a nice,
clean and easy to use high level API.")
    (license license:gpl3+)))

(define-public guile-ffi-fftw
  (let ((commit "294ad9e7491dcb40026d2fec9be2af05263be1c0")
        (revision "2"))
    (package
      (name "guile-ffi-fftw")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lloda/guile-ffi-fftw.git")
                      (commit commit)))
                (file-name (git-file-name "guile-ffi-fftw" version))
                (sha256
                 (base32
                  "08j40a5p6a8pgvhffmzb5rfdnrav2mksy3gfjkdqy93jfj1z5afg"))))
      (build-system guile-build-system)
      (arguments
       `(#:source-directory "mod"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'prepare-build
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "mod/ffi/fftw.scm"
                 (("\\(getenv \"GUILE_FFI_FFTW_LIBFFTW3_PATH\"\\)")
                  (format #f "\"~a/lib\"" (assoc-ref inputs "fftw"))))
               #t))
           (add-after 'build 'check
             (lambda _
               (invoke "guile" "-L" "mod"
                       "-s" "test/test-ffi-fftw.scm"))))))
      (inputs
       `(("fftw" ,fftw)
         ("guile" ,guile-2.2)))
      (home-page "https://github.com/lloda/guile-ffi-fftw/")
      (synopsis "Access FFTW through Guile's FFI")
      (description "This is a minimal set of Guile FFI bindings for the FFTW
library's ‘guru interface’.  It provides two functions: @code{fftw-dft! rank
sign in out} and @code{fftw-dft rank sign in}.  These bindings being minimal,
there is no support for computing & reusing plans, or split r/i transforms, or
anything other than straight complex DFTs.")
      (license license:lgpl3+))))

(define-public srfi-64-driver
  (package
    (name "srfi-64-driver")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.ngyro.com/srfi-64-driver/"
                                  "srfi-64-driver-" version ".tar.gz"))
              (sha256
               (base32
                "188b6mb7sjjg0a8zldikinglf40ky8mg8rwh5768gjmch6gkk3ph"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)))
    (home-page "https://ngyro.com/software/srfi-64-driver.html")
    (synopsis "Automake test driver for SRFI 64 test suites")
    (description "This package provides an Automake test driver that can
run SRFI 64 test suites.  It gives Automake insight into the individual
tests being run, resulting clearer and more specific output.")
    (license license:gpl3+)))

(define-public guile-semver
  (package
    (name "guile-semver")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.ngyro.com/guile-semver/"
                                  "guile-semver-" version ".tar.gz"))
              (sha256
               (base32
                "109p4n39ln44cxvwdccf9kgb96qx54makvd2ir521ssz6wchjyag"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)))
    (home-page "https://ngyro.com/software/guile-semver.html")
    (synopsis "Semantic Versioning (SemVer) for Guile")
    (description "This Guile library provides tools for reading,
comparing, and writing Semantic Versions.  It also includes ranges in
the style of the Node Package Manager (NPM).")
    (license license:gpl3+)))

(define-public guile3.0-semver
  (package
    (inherit guile-semver)
    (name "guile3.0-semver")
    (inputs
     `(("guile" ,guile-3.0)))))

(define-public guile-hashing
  (package
    (name "guile-hashing")
    (version "1.2.0")
    (home-page "https://github.com/weinholt/hashing")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1362d3lmpv7slmv1zmr9wy8panq9sjr9787gc2hagd646mpsfpkl"))))
    (build-system guile-build-system)
    (arguments
     `(#:modules ((guix build guile-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (ice-9 ftw))
       #:implicit-inputs? #f                      ;needs nothing but Guile
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'move-sls-files
                    (lambda _
                      ;; Move the source under hashing/ in order to match
                      ;; module names, and rename .sls files to .scm.
                      (define (target file)
                        (string-append "hashing/" file))

                      (define (sls->scm sls)
                        (string-append (string-drop-right sls 4)
                                       ".scm"))

                      (mkdir "hashing")
                      (for-each (lambda (file)
                                  (rename-file file (sls->scm file)))
                                (find-files "." "\\.sls$"))
                      (for-each (lambda (file)
                                  (rename-file file (target file)))
                                (scandir "." (cut string-suffix? ".scm" <>)))
                      (rename-file "private" "hashing/private")
                      #t)))))
    (native-inputs
     `(("guile" ,guile-2.2)))
    (synopsis "Cryprographic hash functions implemented in Scheme")
    (description
     "The @code{(hashing @dots{})} modules implement cryptographic hash
functions in pure R6RS Scheme: CRC, HMAC, MD5, SHA-1, and SHA-2 (SHA-256,
SHA-512).")
    (license license:expat)))

(define-public guile3.0-hashing
  (package
    (inherit guile-hashing)
    (name "guile3.0-hashing")
    (native-inputs
     `(("guile" ,guile-next)))))

(define-public guile-webutils
  (let ((commit "8541904f761066dc9c27b1153e9a838be9a55299")
        (revision "0"))
    (package
      (name "guile-webutils")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://notabug.org/cwebber/guile-webutils.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1s9n3hbxd7lfpdi0x8wr0cfvlsf6g62ird9gbspxdrp5p05rbi64"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)
         ("texinfo" ,texinfo)))
      (inputs
       `(("guile" ,guile-2.2)))
      (propagated-inputs
       `(("guile-irregex" ,guile-irregex)
         ("guile-gcrypt" ,guile-gcrypt)))
      (home-page "https://notabug.org/cwebber/guile-webutils")
      (synopsis "Web application authoring utilities for Guile")
      (description
       "This package provides tooling to write web applications in Guile, such
as signed sessions, multipart message support, etc.")
      (license license:gpl3+))))

(define-public guile-lens
  (let ((commit "14b15d07255f9d3f55d40a3b750d13c9ee3a154f")
        (revision "0"))
    (package
      (name "guile-lens")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/a-sassmannshausen/guile-lens.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0w8jzqyla56yrgj7acsgd4nspyir6zgp3vgxid4xmwhg9wmf1ida"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'run-hall
             (lambda _
               (setenv "HOME" "/tmp")   ; for ~/.hall
               (invoke "hall" "dist" "-x"))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("guile" ,guile-2.2)
         ("guile-hall" ,guile-hall)
         ("pkg-config" ,pkg-config)
         ("texinfo" ,texinfo)))
      (home-page "https://gitlab.com/a-sassmannshausen/guile-lens.git")
      (synopsis "Composable lenses for data structures in Guile")
      (description
       "Guile-Lens is a library implementing lenses in Guile.  The library is
currently a re-implementation of the lentes library for Clojure.  Lenses
provide composable procedures, which can be used to focus, apply functions
over, or update a value in arbitrary data structures.")
      (license license:gpl3+))))

(define-public guile-xapian
  (let ((commit "ede26b808188eb4d14c6b4181c933dfc09c0a22e")
        (revision "0"))
    (package
      (name "guile-xapian")
      (version (git-version "0" revision commit))
      (home-page "https://git.systemreboot.net/guile-xapian")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference (url home-page)
                             (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "07a9fmqi3pm6mbbpzi01mjwrqwnljs2rnc3603sq49dz4lf663gb"))))
      (build-system gnu-build-system)
      (arguments
       '(#:make-flags '("GUILE_AUTO_COMPILE=0"))) ; to prevent guild warnings
      (inputs
       `(("guile" ,guile-2.2)
         ("xapian" ,xapian)
         ("zlib" ,zlib)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("autoconf-archive" ,autoconf-archive)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)
         ("swig" ,swig)))
      (synopsis "Guile bindings for Xapian")
      (description "@code{guile-xapian} provides Guile bindings for Xapian, a
search engine library.  Xapian is a highly adaptable toolkit which allows
developers to easily add advanced indexing and search facilities to their own
applications.  It has built-in support for several families of weighting
models and also supports a rich set of boolean query operators.")
      (license license:gpl2+))))

(define-public guile3.0-xapian
  (package
    (inherit guile-xapian)
    (name "guile3.0-xapian")
    (inputs
     `(("guile" ,guile-next)
       ,@(alist-delete "guile" (package-inputs guile-xapian))))))

(define-public guile-torrent
  (package
    (name "guile-torrent")
    (version "0.1.3")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url
                           "https://github.com/o-nly/torrent.git")
                          (commit version)))
                    (file-name (git-file-name name version))
                    (sha256
                     (base32
                      "1yiagi55ncq1x7s9n7salzywjm4l96y3n7y3s47a9anvz87mrmim"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("guile" ,guile-2.2)
       ("texinfo" ,texinfo)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("guile-gcrypt" ,guile-gcrypt)))
    (home-page "https://github.com/o-nly/torrent")
    (synopsis "Torrent library for GNU Guile")
    (description "This package provides facilities for working with
@code{.torrent} or metainfo files.  Implements a bencode reader and writer
according to Bitorrent BEP003.")
    (license license:gpl3+)))

(define-public guile-irc
  (let ((commit "375d3bde9c6ae7ccc9d7cc65817966b6fda8f26a")
        (revision "0"))
    (package
      (name "guile-irc")
      (version (git-version "0.3.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rekado/guile-irc.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "113lcckcywrz9060w1c3fnvr8d7crdsjgsv4h47hgmr1slgadl4y"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags '("--enable-gnutls=yes")))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("texinfo" ,texinfo)))
      (inputs
       `(("gnutls" ,gnutls)
         ("guile" ,guile-2.2)))
      (home-page "https://github.com/rekado/guile-irc")
      (synopsis "IRC library for Guile")
      (description "This package provides a Guile library for @dfn{Internet
Relay Chat} (IRC).")
      ;; Some file headers incorrectly say LGPLv2+.
      (license license:lgpl2.1+))))

(define-public guile3.0-websocket
  (let ((commit "c854e0f84a40d972cbd532bbb89c97ca0126a7cf"))
    (package
      (name "guile3.0-websocket")
      (version "0.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://dthompson.us/guile-websocket.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1hymvsfrmq9qxr5cxnsgdz7y757yp1cpsgxmdp3f5wxxxpqgsmzx"))))
      (build-system gnu-build-system)
      (arguments
       '(#:make-flags
         '("GUILE_AUTO_COMPILE=0")
         #:phases
         (modify-phases %standard-phases
           ;; The package was developed for Guile 2.0 and has this version
           ;; hardcoded in the configure.ac and Makefile.am files. Substitute
           ;; 3.0 instead so it can support Guile 3.0.
           (add-after 'unpack 'update-guile-version
             (lambda _
               (substitute* "configure.ac"
                 (("2.0.9") "3.0.0"))
               (substitute* "Makefile.am"
                 (("2.0") "3.0")

                 ;; Install .go files where they belong.
                 (("/ccache") "/site-ccache"))
               #t)))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)))
      (inputs
       `(("guile" ,guile-next)))
      (synopsis "Websocket server/client for Guile")
      (description "Guile-websocket provides an implementation of the
WebSocket protocol as defined by RFC 6455.")
      (home-page "https://git.dthompson.us/guile-websocket.git")
      (license license:lgpl3+))))
