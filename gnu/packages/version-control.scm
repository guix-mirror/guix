;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2016, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2018, 2020, 2021 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015, 2017, 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2020 EuAndreh <eu@euandre.org>
;;; Copyright © 2017, 2018, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2019, 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 John D. Boy <jboy@bius.moe>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2021 Léo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 LibreMiami <packaging-guix@libremiami.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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

(define-module (gnu packages version-control)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cook)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages web)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages sync)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public bazaar
  (package
    (name "bazaar")
    (version "2.7.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://launchpad.net/bzr/"
                          (version-major+minor version) "/" version
                          "/+download/bzr-" version ".tar.gz"))
      (patches (search-patches "bazaar-CVE-2017-14176.patch"))
      (sha256
       (base32
        "1cysix5k3wa6y7jjck3ckq3abls4gvz570s0v0hxv805nwki4i8d"))))
    (build-system python-build-system)
    (inputs
     ;; Note: 'tools/packaging/lp-upload-release' and 'tools/weavemerge.sh'
     ;; require Zsh.
     `(("gettext" ,gettext-minimal)))
    (arguments
     `(#:tests? #f ; no test target
       #:python ,python-2   ; Python 3 apparently not yet supported, see
                            ; https://answers.launchpad.net/bzr/+question/229048
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-mandir
           (lambda _
             (substitute* "setup.py"
                          (("man/man1") "share/man/man1"))
             #t)))))
    (home-page "https://gnu.org/software/bazaar")
    (synopsis "Version control system supporting both distributed and centralized workflows")
    (description
     "GNU Bazaar is a version control system that allows you to record
changes to project files over time.  It supports both a distributed workflow
as well as the classic centralized workflow.")
    (license license:gpl2+)))

(define git-cross-configure-flags
  '("ac_cv_fread_reads_directories=yes"
    "ac_cv_snprintf_returns_bogus=no"
    "ac_cv_iconv_omits_bom=no"))

(define-public git
  (package
   (name "git")
   (version "2.31.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://kernel.org/software/scm/git/git-"
                                version ".tar.xz"))
            (sha256
             (base32
              "10367n5sv4nsgaxy486pbp7nscx34vjk8vrb06jm9ffm8ix42qcz"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("native-perl" ,perl)
      ;; Add bash-minimal explicitly to ensure it comes before bash-for-tests,
      ;; see <https://bugs.gnu.org/39513>.
      ("bash" ,bash-minimal)
      ("bash-for-tests" ,bash)
      ("gettext" ,gettext-minimal)
      ("git-manpages"
       ,(origin
          (method url-fetch)
          (uri (string-append
                "mirror://kernel.org/software/scm/git/git-manpages-"
                version ".tar.xz"))
          (sha256
           (base32
            "00n7vbfmd3ywgjksgwrszwj0l2niba64qkaq07ra4p8mawy483ax"))))
      ;; For subtree documentation.
      ("asciidoc" ,asciidoc-py3)
      ("docbook-xsl" ,docbook-xsl)
      ("xmlto" ,xmlto)
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("curl" ,curl)
      ("expat" ,expat)
      ("openssl" ,openssl)
      ("perl" ,perl)
      ("python" ,python) ; for git-p4
      ("zlib" ,zlib)

      ;; For PCRE support in git grep (USE_LIBPCRE2).
      ("pcre" ,pcre2)

      ;; For 'gitweb.cgi'.
      ("perl-cgi" ,perl-cgi)

      ;; For 'git-svn'.
      ("subversion" ,subversion)
      ("perl-term-readkey" ,perl-term-readkey)

      ;; For 'git-send-email'.
      ("perl-authen-sasl" ,perl-authen-sasl)
      ("perl-net-smtp-ssl" ,perl-net-smtp-ssl)
      ("perl-io-socket-ssl" ,perl-io-socket-ssl)

      ;; For 'git gui', 'gitk', and 'git citool'.
      ("tcl" ,tcl)
      ("tk" ,tk)

      ;; For 'git-credential-libsecret'
      ("glib" ,glib)
      ("libsecret" ,libsecret)))
   (outputs '("out"                               ; the core
              "send-email"                        ; for git-send-email
              "svn"                               ; git-svn
              "credential-netrc"                  ; git-credential-netrc
              "credential-libsecret"              ; git-credential-libsecret
              "subtree"                           ; git-subtree
              "gui"))                             ; gitk, git gui
   (arguments
    `(#:make-flags `("V=1"                        ;more verbose compilation

                     ,(string-append "SHELL_PATH="
                                     (assoc-ref %build-inputs "bash")
                                     "/bin/sh")

                     ;; Tests require a bash with completion support.
                     ,(string-append "TEST_SHELL_PATH="
                                     (assoc-ref %build-inputs "bash-for-tests")
                                     "/bin/bash")

                     "USE_LIBPCRE2=yes"

                     ;; By default 'make install' creates hard links for
                     ;; things in 'libexec/git-core', which leads to huge
                     ;; nars; see <https://bugs.gnu.org/21949>.
                     "NO_INSTALL_HARDLINKS=indeed")

      ;; Make sure the full bash does not end up in the final closure.
      #:disallowed-references (,bash)

      #:test-target "test"

      ;; The explicit --with-tcltk forces the build system to hardcode the
      ;; absolute file name to 'wish'.
      #:configure-flags (list (string-append "--with-tcltk="
                                             (assoc-ref %build-inputs "tk")
                                             "/bin/wish8.6")  ; XXX
                              ,@(if (%current-target-system)
                                    git-cross-configure-flags
                                    '()))

      #:modules ((srfi srfi-1)
                 (srfi srfi-26)
                 ((guix build gnu-build-system) #:prefix gnu:)
                 ,@%gnu-build-system-modules)
      #:phases
      (modify-phases %standard-phases
        ,@(if (%current-target-system)
              ;; The git build system assumes build == host
              `((add-after 'unpack  'use-host-uname_S
                  (lambda _
                    (substitute* "config.mak.uname"
                      (("uname_S := .*" all)
                       (if (equal? ,(%current-target-system) "i586-pc-gnu")
                         "uname_S := GNU\n"
                         all)))
                    #t)))
              ;; We do not have bash-for-tests when cross-compiling.
              `((add-after 'unpack 'modify-PATH
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let ((path (string-split (getenv "PATH") #\:))
                          (bash-full (assoc-ref inputs "bash-for-tests")))
                      ;; Drop the test bash from PATH so that (which "sh") and
                      ;; similar does the right thing.
                      (setenv "PATH" (string-join
                                      (remove (cut string-prefix? bash-full <>) path)
                                      ":"))
                      #t)))))
        ;; Add cross curl-config script to PATH when cross-compiling.
        ,@(if (%current-target-system)
              '((add-before 'configure 'add-cross-curl-config
                   (lambda* (#:key inputs #:allow-other-keys)
                     (setenv "PATH"
                             (string-append (assoc-ref inputs "curl") "/bin:"
                                            (getenv "PATH")))
                     #t)))
              '())
        (add-after 'configure 'patch-makefiles
          (lambda _
            (substitute* "Makefile"
              (("/usr/bin/perl") (which "perl"))
              (("/usr/bin/python") (which "python3")))
            #t))
        (add-after 'configure 'add-PM.stamp
          (lambda _
            ;; Add the "PM.stamp" to avoid "no rule to make target".
            (call-with-output-file "perl/PM.stamp" (const #t))
            #t))
        (add-after 'build 'build-subtree
          (lambda* (#:key inputs #:allow-other-keys)
            (with-directory-excursion "contrib/subtree"
              (substitute* "Makefile"
                ;; Apparently `xmlto' does not bother to looks up the stylesheets
                ;; specified in the XML, unlike the above substitution.  Instead it
                ;; uses a hard-coded URL.  Work around it here, but if this is
                ;; common perhaps we should hardcode this path in xmlto itself.
                (("\\$\\(XMLTO\\) -m \\$\\(MANPAGE_XSL\\)")
                 (string-append "$(XMLTO) -x "
                                (string-append (assoc-ref inputs "docbook-xsl")
                                               "/xml/xsl/docbook-xsl-"
                                               ,(package-version docbook-xsl))
                                "/manpages/docbook.xsl -m $(MANPAGE_XSL)")))
              (invoke "make")
              (invoke "make" "install")
              (invoke "make" "install-doc")
              (substitute* "git-subtree"
                (("/bin/sh") (which "sh"))))
            #t))
        (add-before 'check 'patch-tests
          (lambda _
            (let ((store-directory (%store-directory)))
              ;; These files contain some funny bytes that Guile is unable
              ;; to decode for shebang patching. Just delete them.
              (for-each delete-file '("t/t4201-shortlog.sh"
                                      "t/t7813-grep-icase-iso.sh"))
              ;; Many tests contain inline shell scripts (hooks etc).
              (substitute* (find-files "t" "\\.sh$")
                (("#!/bin/sh") (string-append "#!" (which "sh"))))
              ;; Un-do shebang patching here to prevent checksum mismatch.
              (substitute* '("t/t4034/perl/pre" "t/t4034/perl/post")
                (("^#!.*/bin/perl") "#!/usr/bin/perl"))
              (substitute* "t/t5003-archive-zip.sh"
                (("cp /bin/sh") (string-append "cp " (which "sh"))))
              (substitute* "t/t6030-bisect-porcelain.sh"
                (("\"/bin/sh\"") (string-append "\"" (which "sh") "\"")))
              ;; FIXME: This test runs `git commit` with a bogus EDITOR
              ;; and empty commit message, but does not fail the way it's
              ;; expected to. The test passes when invoked interactively.
              (substitute* "t/t7508-status.sh"
                (("\tcommit_template_commented") "\ttrue"))
              ;; More checksum mismatches due to odd shebangs.
              (substitute* "t/t9100-git-svn-basic.sh"
                (((string-append "\"#!" store-directory ".*/bin/sh")) "\"#!/bin/sh") )
              (substitute* "t/t9300-fast-import.sh"
                (((string-append "\t#!" store-directory ".*/bin/sh")) "\t#!/bin/sh")
                (((string-append "'#!" store-directory ".*/bin/sh")) "'#!/bin/sh"))
              ;; FIXME: Some hooks fail with "basename: command not found".
              ;; See 't/trash directory.t9164.../svn-hook.log'.
              (delete-file "t/t9164-git-svn-dcommit-concurrent.sh")

              ;; XXX: These tests fail intermittently for unknown reasons:
              ;; <https://bugs.gnu.org/29546>.
              (for-each delete-file
                        '("t/t9128-git-svn-cmd-branch.sh"
                          "t/t9167-git-svn-cmd-branch-subproject.sh"
                          "t/t9141-git-svn-multiple-branches.sh"))
              #t)))
        (add-after 'install 'install-shell-completion
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out         (assoc-ref outputs "out"))
                   (completions (string-append out "/etc/bash_completion.d")))
              ;; TODO: Install the tcsh and zsh completions in the right place.
              (mkdir-p completions)
              (copy-file "contrib/completion/git-completion.bash"
                         (string-append completions "/git"))
              #t)))
        (add-after 'install 'install-credential-netrc
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((netrc (assoc-ref outputs "credential-netrc")))
              (install-file "contrib/credential/netrc/git-credential-netrc.perl"
                            (string-append netrc "/bin"))
              (rename-file (string-append netrc "/bin/git-credential-netrc.perl")
                           (string-append netrc "/bin/git-credential-netrc"))
              ;; Previously, Git.pm was automatically found by netrc.
              ;; Perl 5.26 changed how it locates modules so that @INC no
              ;; longer includes the current working directory (the Perl
              ;; community calls this "dotless @INC").
              (wrap-program (string-append netrc "/bin/git-credential-netrc")
                `("PERL5LIB" ":" prefix
                  (,(string-append (assoc-ref outputs "out") "/share/perl5"))))
              #t)))
        (add-after 'install 'install-credential-libsecret
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((libsecret (assoc-ref outputs "credential-libsecret")))
              (with-directory-excursion "contrib/credential/libsecret"
                ((assoc-ref gnu:%standard-phases 'build))
                (install-file "git-credential-libsecret"
                              (string-append libsecret "/bin"))
                #t))))
        (add-after 'install 'install-subtree
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((subtree (assoc-ref outputs "subtree")))
              (install-file "contrib/subtree/git-subtree"
                            (string-append subtree "/bin"))
              (install-file "contrib/subtree/git-subtree.1"
                            (string-append subtree "/share/man/man1"))
              #t)))
         (add-after 'install 'restore-sample-hooks-shebang
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/share/git-core/templates/hooks")))
               (for-each (lambda (file)
                           (format #t "restoring shebang on `~a'~%" file)
                           (substitute* file
                             (("^#!.*/bin/sh") "#!/bin/sh")))
                         (find-files dir ".*"))
               #t)))
        (add-after 'install 'split
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; Split the binaries to the various outputs.
            (let* ((out      (assoc-ref outputs "out"))
                   (se       (assoc-ref outputs "send-email"))
                   (svn      (assoc-ref outputs "svn"))
                   (gui      (assoc-ref outputs "gui"))
                   (gitk     (string-append out "/bin/gitk"))
                   (gitk*    (string-append gui "/bin/gitk"))
                   (git-gui  (string-append out "/libexec/git-core/git-gui"))
                   (git-gui* (string-append gui "/libexec/git-core/git-gui"))
                   (git-cit  (string-append out "/libexec/git-core/git-citool"))
                   (git-cit* (string-append gui "/libexec/git-core/git-citool"))
                   (git-se   (string-append out "/libexec/git-core/git-send-email"))
                   (git-se*  (string-append se  "/libexec/git-core/git-send-email"))
                   (git-svn  (string-append out "/libexec/git-core/git-svn"))
                   (git-svn* (string-append svn "/libexec/git-core/git-svn"))
                   (git-sm   (string-append out
                                            "/libexec/git-core/git-submodule")))
              (mkdir-p (string-append gui "/bin"))
              (mkdir-p (string-append gui "/libexec/git-core"))
              (mkdir-p (string-append se  "/libexec/git-core"))
              (mkdir-p (string-append svn "/libexec/git-core"))

              (for-each (lambda (old new)
                          (copy-file old new)
                          (delete-file old)
                          (chmod new #o555))
                        (list gitk git-gui git-cit git-se git-svn)
                        (list gitk* git-gui* git-cit* git-se* git-svn*))

              ;; Tell 'git-svn' where Subversion and perl-term-readkey are.
              (wrap-program git-svn*
                `("PATH" ":" prefix
                  (,(string-append (assoc-ref inputs "subversion")
                                   "/bin")))
                `("PERL5LIB" ":" prefix
                  ,(map (lambda (i) (string-append (assoc-ref inputs i)
                                                   "/lib/perl5/site_perl"))
                        '("subversion" "perl-term-readkey")))

                ;; XXX: The .so for SVN/Core.pm lacks a RUNPATH, so
                ;; help it find 'libsvn_client-1.so'.
                `("LD_LIBRARY_PATH" ":" prefix
                  (,(string-append (assoc-ref inputs "subversion")
                                   "/lib"))))

              ;; Tell 'git-send-email' where perl modules are.
              (wrap-program git-se*
                `("PERL5LIB" ":" prefix
                  ,(map (lambda (o) (string-append o "/lib/perl5/site_perl"))
                        (list
                         ,@(transitive-input-references
                            'inputs
                            (map (lambda (l)
                                   (assoc l (package-inputs this-package)))
                                 '("perl-authen-sasl"
                                   "perl-net-smtp-ssl"
                                   "perl-io-socket-ssl")))))))

              ;; Tell 'gitweb.cgi' where perl modules are.
              (wrap-program (string-append out "/share/gitweb/gitweb.cgi")
                `("PERL5LIB" ":" prefix
                  ,(map (lambda (o) (string-append o "/lib/perl5/site_perl"))
                        (list
                         ,@(transitive-input-references
                            'inputs
                            (map (lambda (l)
                                   (assoc l (package-inputs this-package)))
                                 '("perl-cgi")))))))

              ;; Tell 'git-submodule' where Perl is.
              (wrap-program git-sm
                `("PATH" ":" prefix
                  (,(string-append (assoc-ref inputs "perl")
                                   "/bin"))))

              #t)))
        (add-after 'split 'install-man-pages
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (man (string-append out "/share/man"))
                   (manpages (assoc-ref inputs "git-manpages")))
              (mkdir-p man)
              (with-directory-excursion man
                (invoke "tar" "xvf" manpages))))))))

   (native-search-paths
    ;; For HTTPS access, Git needs a single-file certificate bundle, specified
    ;; with $GIT_SSL_CAINFO.
    (list (search-path-specification
           (variable "GIT_SSL_CAINFO")
           (file-type 'regular)
           (separator #f)                         ;single entry
           (files '("etc/ssl/certs/ca-certificates.crt")))
          (search-path-specification
           (variable "GIT_EXEC_PATH")
           (separator #f)                         ;single entry
           (files '("libexec/git-core")))))

   (synopsis "Distributed version control system")
   (description
    "Git is a free distributed version control system designed to handle
everything from small to very large projects with speed and efficiency.")
   (license license:gpl2)
   (home-page "https://git-scm.com/")))

(define-public git-minimal
  ;; The size of the closure of 'git-minimal' is two thirds that of 'git'.
  ;; Its test suite runs slightly faster and most importantly it doesn't
  ;; depend on packages that are expensive to build such as Subversion.
  (package
    (inherit git)
    (name "git-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments git)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'patch-makefiles
             (lambda _
               (substitute* "Makefile"
                 (("/usr/bin/perl") (which "perl")))
               #t))
           (delete 'build-subtree)
           (delete 'split)
           (delete 'install-man-pages)
           (delete 'install-subtree)
           (delete 'install-credential-netrc)
           (delete 'install-credential-libsecret)
           (add-after 'install 'remove-unusable-perl-commands
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out     (assoc-ref outputs "out"))
                      (bin     (string-append out "/bin"))
                      (libexec (string-append out "/libexec")))
                 (for-each (lambda (file)
                             (delete-file (string-append libexec
                                                         "/git-core/" file)))
                           '("git-svn" "git-cvsimport" "git-archimport"
                             "git-cvsserver" "git-request-pull"
                             "git-add--interactive" "git-cvsexportcommit"
                             "git-instaweb" "git-send-email"))
                 (delete-file (string-append bin "/git-cvsserver"))

                 ;; These templates typically depend on Perl.  Remove them.
                 (delete-file-recursively
                  (string-append out "/share/git-core/templates/hooks"))

                 ;; Gitweb depends on Perl as well.
                 (delete-file-recursively
                  (string-append out "/share/gitweb"))
                 #t)))))
       ((#:make-flags flags)
        `(delete "USE_LIBPCRE2=yes" ,flags))
       ((#:configure-flags flags)
        `(list
          ,@(if (%current-target-system)
                git-cross-configure-flags
                '())))
       ((#:disallowed-references lst '())
        `(,perl ,@lst))))
    (outputs '("out"))
    (native-inputs
     `(("bash" ,bash-minimal)
       ("bash-for-tests" ,bash)
       ("native-perl" ,perl)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("curl" ,curl)                             ;for HTTP(S) access
       ("expat" ,expat)                           ;for 'git push' over HTTP(S)
       ("openssl" ,openssl)
       ("perl" ,perl)
       ("zlib" ,zlib)))))

(define-public git2cl
  (let ((commit "1d74d4c0d933fc69ed5cec838c73502584dead05"))
    (package
      (name "git2cl")
      (version (string-append "20120919." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.nongnu.org/git/git2cl.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0wnnbm2sjvfj0qrksj89jlnl69miwl0vk3wrrvgvpclgys3na2g1"))))
      (build-system copy-build-system)
      (inputs
       `(("perl" ,perl)))
      (arguments
       `(#:install-plan '(("git2cl" "bin/git2cl"))))
      (home-page "https://savannah.nongnu.org/projects/git2cl")
      (synopsis "Convert Git logs to GNU ChangeLog format")
      (description "@code{git2cl} is a command line tool for converting Git
logs to GNU ChangeLog format.")
      (license license:gpl2+))))

(define-public gitless
  (package
    (name "gitless")
    (version "0.8.8")
    (source
     (origin
       ;; The PyPI package lacks a test suite.  Build directly from git.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gitless-vcs/gitless")
             (commit (string-append "v" version))))
       (sha256
        (base32 "048kl27zjr68hgs70g3l98ci9765wxva6azzrhcdys7nsdd493n6"))
       (file-name (git-file-name name version))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'loosen-requirements
           (lambda _
             (substitute* "setup.py"
               ;; Using Guix's python-pygit2 1.1.0 appears to work fine…
               (("pygit2==") "pygit2>="))
             #t))
         (add-before 'check 'prepare-for-tests
           (lambda _
             ;; Find the 'gl' command.
             (rename-file "gl.py" "gl")
             (setenv "PATH" (string-append (getcwd) ":" (getenv "PATH")))

             ;; The tests try to run git as if it were already set up.
             (setenv "HOME" (getcwd))
             (invoke "git" "config" "--global" "user.email" "git@example.com")
             (invoke "git" "config" "--global" "user.name" "Guix")))
         (replace 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (git (assoc-ref inputs "git")))
               (wrap-program (string-append out "/bin/gl")
                 `("PATH" ":" prefix (,(string-append git "/bin")))
                 `("PYTHONPATH" ":" =
                   (,(string-append out "/lib/python"
                                    ,(version-major+minor
                                      (package-version python))
                                    "/site-packages:")
                    ,(getenv "PYTHONPATH"))))
               #t))))))
    (native-inputs
     `(("git-for-tests" ,git-minimal)))
    (inputs
     `(("git" ,git-minimal)
       ("python-clint" ,python-clint)
       ("python-pygit2" ,python-pygit2)
       ("python-sh" ,python-sh)))
    (home-page "https://gitless.com")
    (synopsis "Simple version control system built on top of Git")
    (description
     "Gitless is a Git-compatible version control system that aims to be easy to
learn and use.  It simplifies the common workflow by committing changes to
tracked files by default and saving any uncommitted changes as part of a branch.

The friendly @command{gl} command-line interface gives feedback and helps you
figure out what to do next.

Gitless is implemented on top of Git and its commits and repositories are
indistinguishable from Git's.  You (or other contributors) can always fall back
on @command{git}, and use any regular Git hosting service.")
    (license license:expat)))

(define-public git-cal
  (package
    (name "git-cal")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/k4rthik/git-cal")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08s9sif3qlk5n2dqpzq5yjczggnqlnxldljspjdqgpfydb2dqg3z"))))
    (build-system perl-build-system)
    (home-page "https://github.com/k4rthik/git-cal/")
    (synopsis "GitHub like contributions calendar for terminal")
    (description "@code{git-cal} is a script to view commits calendar similar
to GitHub contributions calendar.")
    (license license:expat)))

(define-public libgit2
  (package
    (name "libgit2")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libgit2/libgit2/"
                                  "releases/download/v" version
                                  "/libgit2-" version ".tar.gz"))
              (sha256
               (base32
                "1fjdglkh04qv3b4alg621pxa689i0wlf8m7nf2755zawjr2zhwxd"))
              (patches (search-patches "libgit2-mtime-0.patch"))
              (snippet '(begin
                          (delete-file-recursively "deps") #t))
              (modules '((guix build utils)))))
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags
       (list "-DUSE_NTLMCLIENT=OFF" ;TODO: package this
             "-DREGEX_BACKEND=pcre2"
             "-DUSE_HTTP_PARSER=system"
             ,@(if (%current-target-system)
                   `((string-append
                      "-DPKG_CONFIG_EXECUTABLE="
                      (assoc-ref %build-inputs "pkg-config")
                      "/bin/" ,(%current-target-system) "-pkg-config"))
                   '()))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-hardcoded-paths
           (lambda _
             (substitute* "tests/repo/init.c"
               (("#!/bin/sh") (string-append "#!" (which "sh"))))
             (substitute* "tests/clar/fs.h"
               (("/bin/cp") (which "cp"))
               (("/bin/rm") (which "rm")))
             #t))
         ;; Run checks more verbosely, unless we are cross-compiling.
         (replace 'check
           (lambda* (#:key (tests? #t) #:allow-other-keys)
             (if tests?
                 (invoke "./libgit2_clar" "-v" "-Q")
                 ;; Tests may be disabled if cross-compiling.
                 (format #t "Test suite not run.~%")))))))
    (inputs
     `(("libssh2" ,libssh2)
       ("http-parser" ,http-parser)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)))
    (propagated-inputs
     ;; These libraries are in 'Requires.private' in libgit2.pc.
     `(("openssl" ,openssl)
       ("pcre2" ,pcre2)
       ("zlib" ,zlib)))
    (home-page "https://libgit2.org/")
    (synopsis "Library providing Git core methods")
    (description
     "Libgit2 is a portable, pure C implementation of the Git core methods
provided as a re-entrant linkable library with a solid API, allowing you to
write native speed custom Git applications in any language with bindings.")
    ;; GPLv2 with linking exception
    (license license:gpl2)))

(define-public libgit2-0.28
  (package
    (inherit libgit2)
    (version "0.28.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/libgit2/libgit2/releases/"
                            "download/v" version
                            "/libgit2-" version ".tar.gz"))
        (sha256
         (base32
          "0hjgpqjjmkciw1i8jqkx9q2vhdc4fc99qajhrj2bq8ziwsp6hyrb"))
        (patches (search-patches "libgit2-mtime-0.patch"))
        (modules '((guix build utils)))
        (snippet '(begin
                    (delete-file-recursively "deps") #t))))))

(define-public git-crypt
  (package
    (name "git-crypt")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AGWA/git-crypt")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ba5s0fvmd9hhnfhfsjrm40v0qpxfnwc8vmm55m0k4dryzkzx66q"))))
    (build-system gnu-build-system)
    (inputs
     `(("git" ,git)
       ("openssl" ,openssl)))
    (native-inputs
     `(("docbook-xsl" ,docbook-xsl)
       ("libxslt" ,libxslt)))
    (arguments
     `(#:tests? #f ; No tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-makefile
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               (("http://docbook.sourceforge.net/release/xsl/current/manpages/docbook.xsl")
                (string-append (assoc-ref inputs "docbook-xsl")
                               "/xml/xsl/docbook-xsl-"
                               ,(package-version docbook-xsl)
                               "/manpages/docbook.xsl")))
             #t))
         (replace 'build
           (lambda _
             (invoke "make" "ENABLE_MAN=yes")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make" "install"
                       "ENABLE_MAN=yes"
                       (string-append "PREFIX=" out))))))))
    (home-page "https://www.agwa.name/projects/git-crypt/")
    (synopsis "Transparent encryption of files in a git repository")
    (description "git-crypt enables transparent encryption and decryption of
files in a git repository.  Files which you choose to protect are encrypted when
committed, and decrypted when checked out.  git-crypt lets you freely share a
repository containing a mix of public and private content.  git-crypt gracefully
degrades, so developers without the secret key can still clone and commit to a
repository with encrypted files.  This lets you store your secret material (such
as keys or passwords) in the same repository as your code, without requiring you
to lock down your entire repository.")
    (license license:gpl3+)))

(define-public git-remote-gcrypt
  (package
   (name "git-remote-gcrypt")
   (version "1.3")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://git.spwhitton.name/git-remote-gcrypt")
                   (commit version)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "0n8fzvr6y0pxrbvkywlky2bd8jvi0ayp4n9hwi84l1ldmv4a40dh"))))
   (build-system trivial-build-system)
   (arguments
    `(#:modules ((guix build utils))
      #:builder (begin
                  (use-modules (guix build utils))
                  (let* ((source (assoc-ref %build-inputs "source"))
                         (output (assoc-ref %outputs "out"))
                         (bindir (string-append output "/bin")))
                    (install-file (string-append source "/git-remote-gcrypt")
                                  bindir)
                    #t))))
   (home-page "https://spwhitton.name/tech/code/git-remote-gcrypt/")
   (synopsis "Whole remote repository encryption")
   (description "git-remote-gcrypt is a Git remote helper to push and pull from
repositories encrypted with GnuPG.  It works with the standard Git transports,
including repository hosting services like GitLab.

Remote helper programs are invoked by Git to handle network transport.  This
helper handles @code{gcrypt:} URLs that access a remote repository encrypted
with GPG, using our custom format.

Supported locations are local, @code{rsync://} and @code{sftp://}, where the
repository is stored as a set of files, or instead any Git URL where gcrypt
will store the same representation in a Git repository, bridged over arbitrary
Git transport.

The aim is to provide confidential, authenticated Git storage and
collaboration using typical untrusted file hosts or services.")
   (license license:gpl3+)))

(define-public cgit
  (package
    (name "cgit")
    ;; Update the ‘git-source’ input as well.
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://git.zx2c4.com/cgit/snapshot/cgit-"
                    version ".tar.xz"))
              (sha256
               (base32
                "193d990ym10qlslk0p8mjwp2j6rhqa7fq0y1iff65lvbyv914pss"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; XXX: fail to build the in-source git.
       #:test-target "test"
       #:make-flags '("CC=gcc" "SHELL_PATH=sh")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-git
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Unpack the source of git into the 'git' directory.
             (invoke "tar" "--strip-components=1" "-C" "git" "-xf"
                     (assoc-ref inputs "git-source"))))
         (add-after 'unpack 'patch-absolute-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (define (quoted-file-name input path)
               (string-append "\"" input path "\""))
             (substitute* "ui-snapshot.c"
               (("\"gzip\"")
                (quoted-file-name (assoc-ref inputs "gzip") "/bin/gzip"))
               (("\"bzip2\"")
                (quoted-file-name (assoc-ref inputs "bzip2") "/bin/bzip2"))
               (("\"xz\"")
                (quoted-file-name (assoc-ref inputs "xz") "/bin/xz")))

             (substitute* "filters/about-formatting.sh"
               (("$\\(dirname $0\\)") (string-append (assoc-ref outputs "out")
                                                     "/lib/cgit/filters"))
               (("\\| tr") (string-append "| " (which "tr"))))

             (substitute* "filters/html-converters/txt2html"
               (("sed") (which "sed")))

             (substitute* "filters/html-converters/man2html"
               (("groff") (which "groff")))

             (substitute* "filters/html-converters/rst2html"
               (("rst2html\\.py") (which "rst2html.py")))

             #t))
         (delete 'configure) ; no configure script
         (add-after 'build 'build-man
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "doc-man" make-flags)))
         (replace 'install
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (apply invoke
                      "make" "install" "install-man"
                      (string-append "prefix=" out)
                      (string-append "CGIT_SCRIPT_PATH=" out "/share/cgit")
                      make-flags)
               ;; Move the platform-dependent 'cgit.cgi' into lib to get it
               ;; stripped.
               (rename-file (string-append out "/share/cgit/cgit.cgi")
                            (string-append out "/lib/cgit/cgit.cgi"))
               #t)))
         (add-after 'install 'wrap-python-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
              (lambda (file)
                (wrap-program (string-append (assoc-ref outputs "out")
                                             "/lib/cgit/filters/" file)
                  `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))))
              '("syntax-highlighting.py"
                "html-converters/md2html"))
             #t)))))
    (native-inputs
     ;; For building manpage.
     `(("asciidoc" ,asciidoc)
       ("gzip" ,gzip)
       ("bzip2" ,bzip2)
       ("xz" ,xz)))
    (inputs
     `(;; Building cgit requires a Git source tree.
       ("git-source"
        ,(origin
           (method url-fetch)
           ;; cgit is tightly bound to git.  Use GIT_VER from the Makefile,
           ;; which may not match the current (package-version git).
           (uri "mirror://kernel.org/software/scm/git/git-2.25.4.tar.xz")
           (sha256
            (base32 "11am6s46wmn1yll5614smjhzlghbqq6gysgcs64igjr9y5wzpdxq"))))
       ("openssl" ,openssl)
       ("groff" ,groff)
       ("python" ,python)
       ("python-docutils" ,python-docutils)
       ("python-markdown" ,python-markdown)
       ("python-pygments" ,python-pygments)
       ("zlib" ,zlib)))
    (home-page "https://git.zx2c4.com/cgit/")
    (synopsis "Web frontend for git repositories")
    (description
     "CGit is an attempt to create a fast web interface for the Git SCM, using
a built-in cache to decrease server I/O pressure.")
    (license license:gpl2)))

(define-public python-git-multimail
  (package
    (name "python-git-multimail")
    (version "1.5.0.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "git-multimail" version))
       (sha256
        (base32
         "1zkrbsa70anwpw86ysfwalrb7nsr064kygfiyikyq1pl9pcl969y"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "git-multimail/git_multimail.py"
               (("GIT_EXECUTABLE = 'git'")
                (string-append "GIT_EXECUTABLE = '"
                               (assoc-ref inputs "git") "/bin/git"
                               "'"))
               (("/usr/sbin/sendmail")
                (string-append (assoc-ref inputs "sendmail")
                               "/usr/sbin/sendmail")))
             #t)))))
    (inputs
     `(("git" ,git)
       ("sendmail" ,sendmail)))
    (home-page "https://github.com/git-multimail/git-multimail")
    (synopsis "Send notification emails for Git pushes")
    (description
     "This hook sends emails describing changes introduced by pushes to a Git
repository.  For each reference that was changed, it emits one ReferenceChange
email summarizing how the reference was changed, followed by one Revision
email for each new commit that was introduced by the reference change.

This script is designed to be used as a post-receive hook in a Git
repository")
    (license license:gpl2)))

(define-public python-ghp-import
  (package
    (name "python-ghp-import")
    (version "0.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/davisp/ghp-import")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12pmw3zz3i57ljnm0rxdyjqdyhisbvy18mjwkb3bzp5pgzs2f45c"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'install 'install-documentation
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (doc (string-append out "/share/doc"))
                             (licenses (string-append out "/share/licenses")))
                        (install-file "README.md" doc)
                        (install-file "LICENSE" licenses)))))))
    (home-page "https://github.com/davisp/ghp-import")
    (synopsis "Copy directory to the gh-pages branch")
    (description "Script that copies a directory to the gh-pages branch (by
default) of the repository.")

    ;; See <https://bugs.gnu.org/27913>.
    (license (license:non-copyleft
              "https://raw.githubusercontent.com/davisp/ghp-import/master/LICENSE"
              "Tumbolia Public License"))))

(define-public python2-ghp-import
  (package-with-python2
   (strip-python2-variant python-ghp-import)))

(define-public python-gitdb
  (package
    (name "python-gitdb")
    (version "4.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gitdb" version))
              (sha256
               (base32
                "0l113fphn6msjl3cl3kyf332b6lal7daxdd0nfma0x9ipfb013jr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'create-test-repository
                    (lambda _
                      (mkdir "/tmp/testrepo")
                      ;; Some tests require a git repository, so create one.
                      (with-directory-excursion "/tmp/testrepo"
                        (do ((filecount 1 (1+ filecount)))
                            ((> filecount 1000))
                          (call-with-output-file (string-append
                                                  "file" (number->string filecount))
                            (lambda (port)
                              (format port "~a" filecount))))
                        (begin
                         (invoke "git" "init")
                         (invoke "git" "config" "user.name" "Total Git")
                         (invoke "git" "config" "user.email" "git@localhost")
                         (invoke "git" "add" "-A")
                         (invoke "git" "commit" "-q" "-m" "dummy commit")))

                      ;; The repository checkout must be a "bare" clone.
                      (invoke "git" "clone" "--bare" "/tmp/testrepo"
                              "/tmp/testrepo.git")))
                  (replace 'check
                    (lambda _
                      (setenv "GITDB_TEST_GIT_REPO_BASE" "/tmp/testrepo.git")
                      ;; Skip tests that must be run from the gitdb repository.
                      (setenv "TRAVIS" "1")
                      (invoke "nosetests" "-v"))))))
    (propagated-inputs
     `(("python-smmap" ,python-smmap)))
    (native-inputs
     `(("git" ,git)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/gitpython-developers/gitdb")
    (synopsis "Python implementation of the Git object database")
    (description
     "GitDB allows you to access @dfn{bare} Git repositories for reading and
writing.  It aims at allowing full access to loose objects as well as packs
with performance and scalability in mind.  It operates exclusively on streams,
allowing to handle large objects with a small memory footprint.")
    (license license:bsd-3)))

(define-public python-gitpython
  (package
    (name "python-gitpython")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "GitPython" version))
              (sha256
               (base32
                "1jzllsy9lwc9yibccgv7h9naxisazx2n3zmpy21c8n5xhysw69p4"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ;XXX: Tests can only be run within the GitPython repository.
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'embed-git-reference
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "git/cmd.py"
                        (("git_exec_name = \"git\"")
                         (string-append "git_exec_name = \""
                                        (assoc-ref inputs "git")
                                        "/bin/git\"")))
                      #t)))))
    (inputs
     `(("git" ,git)))
    (propagated-inputs
     `(("python-gitdb" ,python-gitdb)))
    (native-inputs
     `(("python-ddt" ,python-ddt)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/gitpython-developers/GitPython")
    (synopsis "Python library for interacting with Git repositories")
    (description
     "GitPython is a python library used to interact with Git repositories,
high-level like git-porcelain, or low-level like git-plumbing.

It provides abstractions of Git objects for easy access of repository data,
and additionally allows you to access the Git repository more directly using
either a pure Python implementation, or the faster, but more resource intensive
@command{git} command implementation.")
    (license license:bsd-3)))

(define-public shflags
  (package
    (name "shflags")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/kward/shflags")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ydx0sb6vz9s2dgp5bd64y7fpzh9qvmlfjxrbmzac8saknijrlly"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; nothing to configure
         (delete 'build)                ; nothing to build
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (src (string-append out "/src")))
               (install-file "shflags" src)
               #t))))))
    (home-page "https://github.com/kward/shflags")
    (synopsis "Command-line flags library for shell scripts")
    (description
     "Shell Flags (shFlags) is a library written to greatly simplify the
handling of command-line flags in Bourne based Unix shell scripts (bash, dash,
ksh, sh, zsh).  Most shell scripts use getopt for flags processing, but the
different versions of getopt on various OSes make writing portable shell
scripts difficult.  shFlags instead provides an API that doesn't change across
shell and OS versions so the script writer can be confident that the script
will work.")
    (license license:lgpl2.1)))

(define-public git-flow
  (package
    (name "git-flow")
    ;; This version has not be officially released yet, so we build it
    ;; directly from the git repository.
    (version "1.12.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/petervanderdoes/gitflow-avh/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13q4mnrxr03wz2dkhzy73j384g299m4d545cnhxcaznvdwfany4h"))))
    (build-system gnu-build-system)
    (inputs `(("shflags" ,shflags)))
    (arguments
     '(#:tests? #f                    ; no tests
       #:make-flags (list (string-append "prefix="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'reset-shFlags-link
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The link points to a file in the shFlags submodule.
             ;; Redirect it to point to our system shFlags.
             (let ((shflags (assoc-ref inputs "shflags")))
               (begin
                 (delete-file "gitflow-shFlags")
                 (symlink (string-append shflags "/src/shflags")
                          "gitflow-shFlags")))))
         (delete 'configure)
         (delete 'build))))
    (home-page "https://nvie.com/posts/a-successful-git-branching-model/")
    (synopsis "Git extensions for Vincent Driessen's branching model")
    (description
     "Vincent Driessen's branching model is a git branching and release
management strategy that helps developers keep track of features, hotfixes,
and releases in bigger software projects.  The git-flow library of git
subcommands helps automate some parts of the flow to make working with it a
lot easier.")
    (license license:bsd-2)))

(define-public stgit
  (package
    (name "stgit")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ctmarinas/stgit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dixgvjlsk3xisj8blzdhh0nphm5zqkjbj081wgsba52z4zq1y0q"))))
    (build-system python-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("git" ,git)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'hard-code-version
           (lambda _
             ;; setup.py tries to cleverly extract the version number from the
             ;; git history, which the source checkout lacks.  Hard-code one.
             (substitute* "setup.py"
               (("get_ver\\(\\)")
                (format #f "'~a'" ,version)))
             #t))
         (add-before 'check 'patch-tests
           (lambda _
             (substitute* (list "t/t1900-mail.sh"
                                "t/t7504-commit-msg-hook.sh")
               (("/bin/sh")
                (which "bash")))
             #t))
         (replace 'check
           (lambda _
             (invoke "make"
                     "PERL_PATH=perl"
                     (string-append "SHELL_PATH=" (which "bash"))
                     "test"))))))
    (home-page "https://stacked-git.github.io/")
    (synopsis "Stacked Git")
    (description
     "StGit is a command-line application that provides functionality similar
to Quilt (i.e., pushing/popping patches to/from a stack), but using Git
instead of @command{diff} and @command{patch}.  StGit stores its patches in a
Git repository as normal Git commits, and provides a number of commands to
manipulate them in various ways.")
    (license license:gpl2)))

(define-public vcsh
  (package
    (name "vcsh")
    (version "1.20151229")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/RichiH/vcsh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1grpj45nbpv4j60vd2kg4rj53zrm0bc0h9l4pfd3c2mwbvywm6ab"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("git" ,git)
       ("perl" ,perl)
       ("perl-test-harness" ,perl-test-harness)
       ("perl-shell-command" ,perl-shell-command)
       ("perl-test-most" ,perl-test-most)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build))
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:test-target "test"))
    (home-page "https://github.com/RichiH/vcsh")
    (synopsis "Version control system for @code{$HOME}")
    (description
     "vcsh version-controls configuration files in several Git repositories,
all in one single directory.  They all maintain their working trees without
clobbering each other or interfering otherwise.  By default, all Git
repositories maintained via vcsh store the actual files in @code{$HOME},
though this can be overridden.")
    (license license:gpl2+)))

(define-public git-test-sequence
  (let ((commit "48e5a2f5a13a5f30452647237e23362b459b9c76"))
    (package
      (name "git-test-sequence")
      (version (string-append "20140312." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;; There are many other scripts in this directory; we
                      ;; are interested in just one for this package.
                      (url "https://github.com/dustin/bindir")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dcq0y16yznbv4k9h8gg90kv1gkn8r8dbvl4m2rpfd7q5nqhn617"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder (begin
                     (use-modules (guix build utils))
                     (let* ((source (assoc-ref %build-inputs "source"))
                            (output (assoc-ref %outputs "out"))
                            (bindir (string-append output "/bin"))
                            (script "git-test-sequence"))
                       (install-file (string-append source "/" script)
                                     bindir)
                       #t))))
      (home-page "https://dustin.sallings.org/2010/03/28/git-test-sequence.html")
      (synopsis "Run a command over a sequence of commits")
      (description
       "git-test-sequence is similar to an automated git bisect except it’s
linear.  It will test every change between two points in the DAG.  It will
also walk each side of a merge and test those changes individually.")
      (license (license:x11-style "file://LICENSE")))))

(define-public gitolite
  (package
    (name "gitolite")
    (version "3.6.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sitaramc/gitolite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05xw1pmagvkrbzga5pgl3xk9qyc6b5x73f842454f3w9ijspa8zy"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'build)
                  (add-before 'install 'patch-scripts
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((perl (string-append (assoc-ref inputs "perl")
                                                 "/bin/perl")))
                        ;; This seems to take care of every shell script that
                        ;; invokes Perl.
                        (substitute* (find-files "." ".*")
                          ((" perl -")
                           (string-append " " perl " -")))

                        (substitute* (find-files "src/triggers" ".*")
                          ((" sed ")
                           (string-append " " (which "sed") " ")))

                        (substitute*
                            '("src/triggers/post-compile/update-gitweb-access-list"
                              "src/triggers/post-compile/ssh-authkeys-split"
                              "src/triggers/upstream")
                          ((" grep ")
                           (string-append " " (which "grep") " ")))

                        ;; Avoid references to the store in authorized_keys.
                        ;; This works because gitolite-shell is in the PATH.
                        (substitute* "src/triggers/post-compile/ssh-authkeys"
                          (("\\$glshell \\$user")
                           "gitolite-shell $user"))
                        #t)))
                  (add-before 'install 'patch-source
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Gitolite uses cat to test the readability of the
                      ;; pubkey
                      (substitute* "src/lib/Gitolite/Setup.pm"
                        (("\"cat ")
                         (string-append "\"" (which "cat") " "))
                        (("\"ssh-keygen")
                         (string-append "\"" (which "ssh-keygen"))))

                      (substitute* '("src/lib/Gitolite/Hooks/PostUpdate.pm"
                                     "src/lib/Gitolite/Hooks/Update.pm")
                        (("/usr/bin/perl")
                         (string-append (assoc-ref inputs "perl")
                                        "/bin/perl")))

                      (substitute* "src/lib/Gitolite/Common.pm"
                        (("\"ssh-keygen")
                         (string-append "\"" (which "ssh-keygen")))
                        (("\"logger\"")
                         (string-append "\""
                                        (assoc-ref inputs "inetutils")
                                        "/bin/logger\"")))

                      #t))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((output (assoc-ref outputs "out"))
                             (sharedir (string-append output "/share/gitolite"))
                             (bindir (string-append output "/bin")))
                        (mkdir-p sharedir)
                        (mkdir-p bindir)
                        (invoke "./install" "-to" sharedir)
                        ;; Create symlinks for executable scripts in /bin.
                        (for-each (lambda (script)
                                    (symlink (string-append sharedir "/" script)
                                             (string-append bindir "/" script)))
                                  '("gitolite" "gitolite-shell"))
                        #t)))
                  (add-after 'install 'wrap-scripts
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (coreutils (assoc-ref inputs "coreutils"))
                            (findutils (assoc-ref inputs "findutils"))
                            (git (assoc-ref inputs "git")))
                        (wrap-program (string-append out "/bin/gitolite")
                          `("PATH" ":" prefix
                            ,(map (lambda (dir)
                                    (string-append dir "/bin"))
                                  (list out coreutils findutils git))))
                        #t))))))
    (inputs
     `(("perl" ,perl)
       ("coreutils" ,coreutils)
       ("findutils" ,findutils)
       ("inetutils" ,inetutils)))
    ;; git and openssh are propagated because trying to patch the source via
    ;; regexp matching is too brittle and prone to false positives.
    (propagated-inputs
     `(("git" ,git)
       ("openssh" ,openssh)))
    (home-page "https://gitolite.com")
    (synopsis "Git access control layer")
    (description
     "Gitolite is an access control layer on top of Git, providing fine access
control to Git repositories.")
    (license license:gpl2)))

(define-public pre-commit
  (package
    (name "pre-commit")
    (version "2.8.1")
    (source
     (origin
       ;; No tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pre-commit/pre-commit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b3ks6viccq3n4p8i8zgfd40vp1k5nkhmmlz7p4nxcdizw8zxgn8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-up-git
           (lambda _
             ;; Change from /homeless-shelter to /tmp for write permission.
             (setenv "HOME" "/tmp")
             ;; Environment variables used in the tests.
             (setenv "GIT_AUTHOR_NAME" "Your Name")
             (setenv "GIT_COMMITTER_NAME" "Your Name")
             (setenv "GIT_AUTHOR_EMAIL" "you@example.com")
             (setenv "GIT_COMMITTER_EMAIL" "you@example.com")
             (invoke "git" "config" "--global" "user.name" "Your Name")
             (invoke "git" "config" "--global" "user.email" "you@example.com")
           #t))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "tests" "-k"
                     (string-append
                     ;; Disable conda tests.
                      "not test_conda_hook"
                      " and not test_conda_with_additional_dependencies_hook"
                      " and not test_local_conda_additional_dependencies"
                      ;; Disable cpan tests.
                      " and not test_local_perl_additional_dependencies"
                      " and not test_perl_hook"
                      ;; Disable Ruby tests.
                      " and not test_additional_ruby_dependencies_installed"
                      " and not test_install_rbenv"
                      " and not test_install_rbenv_with_version"
                      " and not test_run_a_ruby_hook"
                      " and not test_run_ruby_hook_with_disable_shared_gems"
                      " and not test_run_versioned_ruby_hook"
                      ;; Disable Cargo tests.
                      " and not test_additional_rust_cli_dependencies_installed"
                      " and not test_additional_rust_lib_dependencies_installed"
                      " and not test_local_rust_additional_dependencies"
                      " and not test_rust_hook"
                      ;; Disable dotnet tests.
                      " and not test_dotnet_hook"
                      ;; Disable nodejs tests.
                      " and not test_unhealthy_if_system_node_goes_missing"
                      " and not test_installs_without_links_outside_env"
                      " and not test_healthy_system_node"
                      ;; Disable python2 test.
                      " and not test_switch_language_versions_doesnt_clobber"
                      ;; These tests try to open a network socket.
                      " and not test_additional_golang_dependencies_installed"
                      " and not test_additional_node_dependencies_installed"
                      " and not test_golang_hook"
                      " and not test_golang_hook_still_works_when_gobin_is_set"
                      " and not test_local_golang_additional_dependencies"
                      " and not test_main"
                      " and not test_node_hook_with_npm_userconfig_set"
                      " and not test_run_a_node_hook"
                      " and not test_run_versioned_node_hook"
                      ;; Tests failing with a permission error.
                      ;; They try to write to the filesystem.
                      " and not test_autoupdate_hook_disappearing_repo"
                      " and not test_hook_disppearing_repo_raises"
                      " and not test_img_conflict"
                      " and not test_img_something_unstaged"
                      " and not test_installed_from_venv"
                      " and not test_too_new_version"
                      " and not test_try_repo_uncommitted_changes"
                      " and not test_versions_ok"
                      ;; This test tries to activate a virtualenv.
                      " and not test_healthy_venv_creator"
                      ;; Fatal error: Not a Git repository.
                      " and not test_all_cmds"
                      " and not test_try_repo"
                      ;; No module named 'pip._internal.cli.main'.
                      " and not test_additional_dependencies_roll_forward"
                      ;; Assertion errors.
                      " and not test_install_existing_hooks_no_overwrite"
                      " and not test_uninstall_restores_legacy_hooks"))))
         (add-before 'reset-gzip-timestamps 'make-files-writable
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure .gz files are writable so that the
             ;; 'reset-gzip-timestamps' phase can do its work.
             (let ((out (assoc-ref outputs "out")))
               (for-each make-file-writable
                         (find-files out "\\.gz$"))
               #t))))))
    (native-inputs
     `(("git" ,git-minimal)
       ("python-pytest" ,python-pytest)
       ("python-re-assert" ,python-re-assert)))
    ;; Propagate because pre-commit is also used as a module.
    (propagated-inputs
     `(("python-cfgv" ,python-cfgv)
       ("python-identify" ,python-identify)
       ("python-nodeenv" ,python-nodeenv)
       ("python-pyyaml" ,python-pyyaml)
       ("python-toml" ,python-toml)
       ("python-virtualenv" ,python-virtualenv)))
    (home-page "https://pre-commit.com/")
    (synopsis "Framework for managing and maintaining multi-language pre-commit hooks")
    (description
     "Pre-commit is a multi-language package manager for pre-commit hooks.  You
specify a list of hooks you want and pre-commit manages the installation and
execution of any hook written in any language before every commit.")
    (license license:expat)))

(define-public mercurial
  (package
    (name "mercurial")
    (version "5.6.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://www.mercurial-scm.org/"
                                 "release/mercurial-" version ".tar.gz"))
             (sha256
              (base32
               "1bgz8f1a7lnmh6lzcvwg6q1yx6i7yibhwy06l4k55i04957jap75"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* '("tests/test-extdiff.t"
                            "tests/test-logtoprocess.t"
                            "tests/test-patchbomb.t"
                            "tests/test-run-tests.t"
                            "tests/test-transplant.t")
               (("/bin/sh")
                (which "sh")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (with-directory-excursion "tests"
               ;; The following tests are known to fail.
               (for-each delete-file
                         '(;; XXX: This test calls 'run-tests.py --with-hg=
                           ;; `which hg`' and fails because there is no hg on
                           ;; PATH from before (that's why we are building it!)?
                           "test-hghave.t"

                           ;; These tests fail because the program is not
                           ;; connected to a TTY in the build container.
                           "test-nointerrupt.t"
                           "test-transaction-rollback-on-sigpipe.t"

                           ;; FIXME: This gets killed but does not receive an interrupt.
                           "test-commandserver.t"

                           ;; Only works when run in a hg-repo, not in an
                           ;; extracted tarball
                           "test-doctest.py"

                           ;; TODO: the fqaddr() call fails in the build
                           ;; container, causing these server tests to fail.
                           "test-hgwebdir.t"
                           "test-http-branchmap.t"
                           "test-pull-bundle.t"
                           "test-push-http.t"
                           "test-serve.t"
                           "test-subrepo-deep-nested-change.t"
                           "test-subrepo-recursion.t"))
               (when tests?
                 (invoke "./run-tests.py"
                         ;; ‘make check’ does not respect ‘-j’.
                         (string-append "-j" (number->string
                                              (parallel-job-count)))
                         ;; The default time-outs are too low for many systems.
                         ;; Raise them generously: Guix enforces its own.
                         "--timeout" "86400"
                         "--slowtimeout" "86400"
                         ;; The test suite takes a long time and produces little
                         ;; output by default.  Prevent timeouts due to silence.
                         "-v"))))))))
    ;; The following inputs are only needed to run the tests.
    (native-inputs
     `(("python-nose" ,python-nose)
       ("unzip" ,unzip)
       ("which" ,which)))
    (home-page "https://www.mercurial-scm.org/")
    (synopsis "Decentralized version control system")
    (description
     "Mercurial is a free, distributed source control management tool.
It efficiently handles projects of any size
and offers an easy and intuitive interface.")
    (license license:gpl2+)))

(define-public python-hg-evolve
  (package
    (name "python-hg-evolve")
    (version "10.0.1")
    (source
      (origin
        (method hg-fetch)
        (uri (hg-reference
               (url "https://www.mercurial-scm.org/repo/evolve")
               (changeset version)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
          (base32
            "1lz407373lfam9n02gq0l0rc2sjvn0m96kbzy93ipia3ika8fa68"))))
    (build-system python-build-system)
    (arguments
     ;; Tests need mercurial source code.
     '(#:tests? #f))
    (propagated-inputs
      `(("mercurial" ,mercurial)))
    (home-page "https://www.mercurial-scm.org/doc/evolution/")
    (synopsis "Flexible evolution of Mercurial history")
    (description "Evolve is a Mercurial extension for faster and safer mutable
history.  It implements the changeset evolution concept for Mercurial.")
    (license license:gpl2)))

(define-public neon
  (package
    (name "neon")
    (version "0.31.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://notroj.github.io/neon/neon-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0y46dbhiblcvg8k41bdydr3fivghwk73z040ki5825d24ynf67ng"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxml2" ,libxml2)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (arguments
     `(;; FIXME: Add tests once reverse address lookup is fixed in glibc, see
       ;; https://sourceware.org/bugzilla/show_bug.cgi?id=16475
       #:tests? #f
       #:configure-flags '("--enable-shared"
                           "--disable-static"
                           ;; requires libgnutils-config, deprecated
                           ;; in gnutls 2.8.
                           ; "--with-ssl=gnutls")))
                           "--with-ssl=openssl")))
    (home-page "https://notroj.github.io/neon/")
    (synopsis "HTTP and WebDAV client library")
    (description
     "Neon is an HTTP and WebDAV client library, with a C interface and the
following features:
@enumerate
@item High-level wrappers for common HTTP and WebDAV operations (GET, MOVE,
  DELETE, etc.);
@item low-level interface to the HTTP request/response engine, allowing the use
  of arbitrary HTTP methods, headers, etc.;
@item authentication support including Basic and Digest support, along with
  GSSAPI-based Negotiate on Unix, and SSPI-based Negotiate/NTLM on Win32;
@item SSL/TLS support using OpenSSL or GnuTLS, exposing an abstraction layer for
  verifying server certificates, handling client certificates, and examining
  certificate properties, smartcard-based client certificates are also
  supported via a PKCS#11 wrapper interface;
@item abstract interface to parsing XML using libxml2 or expat, and wrappers for
  simplifying handling XML HTTP response bodies;
@item WebDAV metadata support, wrappers for PROPFIND and PROPPATCH to simplify
  property manipulation.
@end enumerate\n")
    (license license:gpl2+))) ; for documentation and tests; source under lgpl2.0+

(define-public subversion
  (package
    (name "subversion")
    (version "1.14.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://apache/subversion/"
                                 "subversion-" version ".tar.bz2"))
             (sha256
              (base32
               "1ag1hvcm9q92kgalzbbgcsq9clxnzmbj9nciz9lmabjx4lyajp9c"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-tests? #f             ; TODO Seems to cause test failures on
                                        ; i686-linux
       #:configure-flags '("--enable-static=no")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-libtool-wrapper-ls
           (lambda* (#:key inputs #:allow-other-keys)
             ;; This substitution allows tests svnauthz_tests and svnlook_tests
             ;; to pass.  These tests execute svnauthz and svnlook through
             ;; their libtool wrapper scripts from svn hooks, whose empty
             ;; environments cause "ls: command not found" errors.  It would be
             ;; nice if this fix ultimately made its way into libtool.
             (let ((coreutils (assoc-ref inputs "coreutils")))
               (substitute* "libtool"
                 (("\\\\`ls") (string-append "\\`" coreutils "/bin/ls")))
               #t)))
         (add-before 'build 'patch-test-sh
           (lambda _
             (substitute* "subversion/tests/libsvn_repos/repos-test.c"
               (("#!/bin/sh") (string-append "#!" (which "sh"))))
             #t))
         (add-before 'check 'set-PARALLEL
           (lambda* (#:key parallel-tests? #:allow-other-keys)
             (if parallel-tests?
                 (setenv "PARALLEL" (number->string (parallel-job-count)))
                 (simple-format #t "parallel-tests? are disabled\n"))
             #t))
         (add-after 'install 'install-perl-bindings
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Follow the instructions from 'subversion/bindings/swig/INSTALL'.
             (let ((out (assoc-ref outputs "out")))
               (invoke "make" "swig-pl-lib")
               ;; FIXME: Test failures.
               ;; (invoke "make" "check-swig-pl")
               (invoke "make" "install-swig-pl-lib")

               ;; Set the right installation prefix.
               (with-directory-excursion
                   "subversion/bindings/swig/perl/native"
                 (invoke "perl" "Makefile.PL"
                         "NO_PERLLOCAL=1"
                         (string-append "PREFIX=" out))
                 (invoke "make" "install"
                         (string-append "OTHERLDFLAGS="
                                        "-Wl,-rpath="
                                        out "/lib")))))))))
    (native-inputs
      `(("pkg-config" ,pkg-config)
        ;; For the Perl bindings.
        ("swig" ,swig)))
    (inputs
      `(("apr" ,apr)
        ("apr-util" ,apr-util)
        ("lz4" ,lz4)
        ("serf" ,serf)
        ("perl" ,perl)
        ("python" ,python-wrapper)
        ("sqlite" ,sqlite)
        ("utf8proc" ,utf8proc)
        ("zlib" ,zlib)))
    (home-page "https://subversion.apache.org/")
    (synopsis "Revision control system")
    (description
     "@dfn{Subversion} (svn) exists to be recognized and adopted as a
centralized version control system characterized by its
reliability as a safe haven for valuable data; the simplicity of its model and
usage; and its ability to support the needs of a wide variety of users and
projects, from individuals to large-scale enterprise operations.")
    (license license:asl2.0)))

(define-public rcs
  (package
    (name "rcs")
    (version "5.10.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/rcs/rcs-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1if5pa4iip2p70gljm54nggfdnsfjxa4cqz8fpj07lvsijary39s"))
             (patches (search-patches "rcs-5.10.0-no-stdin.patch"))))
    (build-system gnu-build-system)
    (native-inputs `(("ed" ,ed)))
    (home-page "https://www.gnu.org/software/rcs/")
    (synopsis "Per-file local revision control system")
    (description
     "RCS is the original Revision Control System.  It works on a
file-by-file basis, in contrast to subsequent version control systems such as
CVS, Subversion, and Git.  This can make it suitable for system
administration files, for example, which are often inherently local to one
machine.")
    (license license:gpl3+)))

(define-public cvs
  (package
    (name "cvs")
    (version "1.12.13")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://ftp.gnu.org/non-gnu/cvs/source/feature/"
                   version "/cvs-" version ".tar.bz2"))
             (patches (search-patches "cvs-CVE-2017-12836.patch"))
             (sha256
              (base32
               "0pjir8cwn0087mxszzbsi1gyfc6373vif96cw4q3m1x6p49kd1bq"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The test suite looks flawed, and the package is obsolete anyway.
     '(#:tests? #f
       #:configure-flags (list "--with-external-zlib")))
    (inputs `(("zlib" ,zlib)
              ("nano" ,nano)))                    ; the default editor
    (home-page "http://cvs.nongnu.org")
    (synopsis "Historical centralized version control system")
    (description
     "CVS is a version control system, an important component of Source
Configuration Management (SCM).  Using it, you can record the history of
sources files, and documents.  It fills a similar role to the free software
RCS, PRCS, and Aegis packages.")
    (license license:gpl1+)))

(define-public cvs-fast-export
  (package
    (name "cvs-fast-export")
    (version "1.55")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.catb.org/~esr/cvs-fast-export/"
                                  "cvs-fast-export-" version ".tar.gz"))
              (sha256
               (base32
                "06y2myhhv2ap08bq7d7shq0b7lq6wgznwrpz6622xq66cxkf2n5g"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure))       ; no configure script
       #:parallel-build? #f         ; parallel a2x commands fail spectacularly
       #:make-flags
       (list "CC=gcc" (string-append "prefix?=" (assoc-ref %outputs "out")))))
    (inputs
     `(("git" ,git)
       ("python" ,python-wrapper)))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ;; These are needed for the tests.
       ("cvs" ,cvs)
       ("rcs" ,rcs)))
    (home-page "http://www.catb.org/esr/cvs-fast-export/")
    (synopsis "Export an RCS or CVS history as a fast-import stream")
    (description "This program analyzes a collection of RCS files in a CVS
repository (or outside of one) and, when possible, emits an equivalent history
in the form of a fast-import stream.  Not all possible histories can be
rendered this way; the program tries to emit useful warnings when it can't.

The program can also produce a visualization of the resulting commit directed
acyclic graph (DAG) in the input format of @uref{http://www.graphviz.org,
Graphviz}.  The package also includes @command{cvssync}, a tool for mirroring
masters from remote CVS hosts.")
    (license license:gpl2+)))

(define-public vc-dwim
  (package
    (name "vc-dwim")
    (version "1.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/vc-dwim/vc-dwim-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0am6axxdvkm2vwgg0gjrd930yv4dlsdbf0rdv0zh5bhy1ir64rph"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (native-inputs
     `(("emacs" ,emacs-minimal)     ; for `ctags'
       ("inetutils" ,inetutils)))   ; for `hostname', used in the tests
    (home-page "https://www.gnu.org/software/vc-dwim/")
    (synopsis "Version-control-agnostic ChangeLog diff and commit tool")
    (description
     "The vc-dwim package contains two tools, \"vc-dwim\" and \"vc-chlog\".
vc-dwim is a tool that simplifies the task of maintaining a ChangeLog and
using version control at the same time, for example by printing a reminder
when a file change has been described in the ChangeLog but the file has not
been added to the VC.  vc-chlog scans changed files and generates
standards-compliant ChangeLog entries based on the changes that it detects.")
    (license license:gpl3+)))

(define-public diffstat
  (package
    (name "diffstat")
    (version "1.64")
    (source (origin
              (method url-fetch)
              (uri
               (list
                 (string-append "ftp://invisible-island.net/diffstat/"
                                "diffstat-" version ".tgz")
                 (string-append "http://invisible-mirror.net/archives/diffstat/"
                                "diffstat-" version ".tgz")))
              (sha256
               (base32
                "1z7pwcv48fjnhxrjcsjdy83x8b9ckl582mbbds90a79fkn6y7bmq"))))
    (build-system gnu-build-system)
    (home-page "https://invisible-island.net/diffstat/")
    (synopsis "Make histograms from the output of @command{diff}")
    (description
     "Diffstat reads the output of @command{diff} and displays a histogram of
the insertions, deletions, and modifications per file.  It is useful for
reviewing large, complex patch files.")
    (license (license:x11-style "file://COPYING"))))

(define-public cssc
  (package
    (name "cssc")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/" name "/CSSC-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1vsisqq573xjr2qpn19iwmpqgl3mq03m790akpa4rvj60b4d1gni"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'precheck
           (lambda _
             (begin
               (substitute* "tests/common/test-common"
                 (("/bin/pwd") (which "pwd")))

               (substitute* "tests/prt/all-512.sh"
                 (("/bin/sh") (which "sh")))

               ;; XXX: This test has no hope of passing until there is a "nogroup"
               ;; entry (or at least some group to which the guix builder does
               ;; not belong) in the /etc/group file of the build environment.
               ;; Currently we do not have such a group.  Disable this test for now.
               (substitute* "tests/Makefile"
                 (("test-delta ") ""))))))))
    ;; These are needed for the tests
    (native-inputs `(("git" ,git)
                     ("cvs" ,cvs)))
    (home-page "https://www.gnu.org/software/cssc/")
    (synopsis "File-based version control like SCCS")
    (description  "GNU CSSC provides a replacement for the legacy Unix source
code control system SCCS.  This allows old code still under that system to be
accessed and migrated on modern systems.")
    (license license:gpl3+)))

;; This package can unfortunately work only in -TEST mode, since Aegis
;; requires that it is installed setuid root.
(define-public aegis
  (package
    (name "aegis")
    (version "4.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/aegis/aegis/" version
                                  "/aegis-" version ".tar.gz"))
              (sha256
               (base32
                "18s86ssarfmc4l17gbpzybca29m5wa37cbaimdji8czlcry3mcjl"))
            (patches (search-patches "aegis-perl-tempdir1.patch"
                                     "aegis-perl-tempdir2.patch"
                                     "aegis-test-fixup-1.patch"
                                     "aegis-test-fixup-2.patch"
                                     "aegis-constness-error.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("e2fsprogs" ,e2fsprogs)
       ("curl" ,curl)
       ("file" ,file)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)
       ("gettext" ,gettext-minimal)))
    (native-inputs
     `(("bison" ,bison)
       ("groff" ,groff)
       ("perl" ,perl)
       ;; Various tests require the following:
       ("cvs" ,cvs)
       ("flex" ,flex)
       ("cook" ,cook)
       ("subversion" ,subversion)
       ("rcs" ,rcs)
       ("ed" ,ed)))
    (arguments
     `(#:configure-flags (list "--with-no-aegis-configured"
                               "--sharedstatedir=/var/com/aegis"
                               ;; Uses the old 'throw()' specifier with 'new'
                               ;; which changed in C++11.
                               "CXXFLAGS=-std=c++03")
       #:parallel-build? #f ; There are some nasty racy rules in the Makefile.
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-conf
           (lambda _
              (substitute* (append '("configure"
                                     "etc/check-tar-gz.sh"
                                     "etc/patches.sh"
                                     "etc/test.sh"
                                     "script/aexver.in"
                                     "script/aebisect.in"
                                     "script/aeintegratq.in"
                                     "script/tkaegis.in"
                                     "script/test_funcs.in"
                                     "web/eg_oss_templ.sh"
                                     "web/webiface.html"
                                     "libaegis/getpw_cache.cc")
                                   (find-files "test" "\\.sh"))
                           (("/bin/sh") (which "sh")))
              (setenv "SH" (which "sh"))
              #t))
         (replace 'check
           (lambda _
             (let ((home (string-append (getcwd) "/my-new-home")))
               ;; Some tests need to write to $HOME.
               (mkdir home)
               (setenv "HOME" home)

               ;; This test assumes that flex has been symlinked to "lex".
               (substitute* "test/00/t0011a.sh"
                 (("type lex")  "type flex"))

               ;; XXX Disable tests that fail, for unknown reasons, ‘for now’.
               (for-each
                (lambda (test) (substitute* "Makefile"
                                 (((string-append "test/" test "\\.ES ")) "")))
                (list "00/t0011a"
                      "00/t0049a"
                      "01/t0196a"))

               ;; The author decided to call the check rule "sure".
               (invoke "make" "sure")))))))
    (home-page "https://sourceforge.net/projects/aegis/")
    (synopsis "Project change supervisor")
    (description "Aegis is a project change supervisor, and performs some of
the Software Configuration Management needed in a CASE environment.  Aegis
provides a framework within which a team of developers may work on many
changes to a program independently, and Aegis coordinates integrating these
changes back into the master source of the program, with as little disruption
as possible.  Resolution of contention for source files, a major headache for
any project with more than one developer, is one of Aegis's major functions.")
    (license license:gpl3+)))

(define-public reposurgeon
  (package
    (name "reposurgeon")
    (version "3.43")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.catb.org/~esr/" name "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1af0z14wcm4bk5a9ysinbwq2fp3lf5f7i8mvwh7286hr3fnagcaz"))
              (patches (search-patches
                        "reposurgeon-add-missing-docbook-files.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "ECHO=echo"
             (string-append "target=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-inputs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((tzdata (assoc-ref inputs "tzdata")))
               (substitute* "reposurgeon"
                 (("/usr/share/zoneinfo")
                  (string-append tzdata "/share/zoneinfo")))
               (substitute* "test/svn-to-svn"
                 (("/bin/echo") "echo"))
               #t)))
         (delete 'configure)            ; no configure script
         (add-before 'build 'fix-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (find-files "." "\\.xml$")
               (("docbook/docbookx.dtd")
                (string-append (assoc-ref inputs "docbook-xml")
                               "/xml/dtd/docbook/docbookx.dtd")))
             #t))
         (add-before 'check 'set-up-test-environment
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((tzdata (assoc-ref inputs "tzdata")))
               (setenv "TZDIR" (string-append tzdata "/share/zoneinfo"))
               #t)))
         (add-after 'install 'install-emacs-data
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "reposurgeon-mode.el"
                           (string-append (assoc-ref outputs "out")
                                          "/share/emacs/site-lisp"))
             #t)))))
    (inputs
     `(("python" ,python-wrapper)
       ("tzdata" ,tzdata)))
    (native-inputs
     `( ;; For building documentation.
       ("asciidoc" ,asciidoc)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("libxml2" ,libxml2)
       ("xmlto" ,xmlto)

       ;; For tests.
       ("cvs" ,cvs)
       ("git" ,git)
       ("mercurial" ,mercurial)
       ("subversion" ,subversion)))
    (home-page "http://www.catb.org/~esr/reposurgeon/")
    (synopsis "Edit version-control repository history")
    (description "Reposurgeon enables risky operations that version-control
systems don't want to let you do, such as editing past comments and metadata
and removing commits.  It works with any version control system that can
export and import Git fast-import streams, including Git, Mercurial, Fossil,
Bazaar, CVS, RCS, and Src.  It can also read Subversion dump files directly
and can thus be used to script production of very high-quality conversions
from Subversion to any supported Distributed Version Control System (DVCS).")
    ;; Most files are distributed under bsd-2, except 'repocutter' which is
    ;; under bsd-3.
    (license (list license:bsd-2 license:bsd-3))))

(define-public tig
  (package
    (name "tig")
    (version "2.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/jonas/tig/releases/download/tig-"
                    version "/tig-" version ".tar.gz"))
              (sha256
               (base32
                "1p1575yh4daxjifywxkd0hgyfwciylqcm2qakawvwn6mk620ca75"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("xmlto" ,xmlto)))
    (inputs
     `(("ncurses" ,ncurses)
       ("readline" ,readline)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda _
             (invoke "make" "install-doc"))))
       #:test-target "test"
       #:tests? #f))                    ; tests require access to /dev/tty
    (home-page "https://jonas.github.io/tig/")
    (synopsis "Ncurses-based text user interface for Git")
    (description
     "Tig is an ncurses text user interface for Git, primarily intended as
a history browser.  It can also stage hunks for commit, or colorize the
output of the @code{git} command.")
    (license license:gpl2+)))

(define-public findnewest
  (package
    (name "findnewest")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/0-wiz-0/findnewest")
             (commit (string-append "findnewest-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x1cbn2b27h5r0ah5xc06fkalfdci2ngrgd4wibxjw0h88h0nvgq"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)))
    (home-page "https://github.com/0-wiz-0/findnewest/releases")
    (synopsis "Print the modification time of the latest file")
    (description
     "Recursively find the newest file in a file tree and print its
modification time.")
    (license license:bsd-2)))

(define-public myrepos
  (package
    (name "myrepos")
    (version "1.20180726")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://myrepos.branchable.com/myrepos")
             (commit version)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "0jphw61plm8cgklja6hs639xhdvxgvjwbr6jpvjwpp7hc5gmhms5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:make-flags (list (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'wrap-webcheckout
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/webcheckout")
                 `("PERL5LIB" ":" prefix
                   ,(map (lambda (i) (string-append (assoc-ref inputs i)
                                                    "/lib/perl5/site_perl"))
                         '("perl-encode-locale" "perl-http-date"
                           "perl-http-message" "perl-html-parser" "perl-libwww"
                           "perl-uri" "perl-try-tiny"))))
               #t))))))
    (inputs
     `(("perl" ,perl)
       ("perl-encode-locale" ,perl-encode-locale)
       ("perl-html-parser" ,perl-html-parser)
       ("perl-http-date" ,perl-http-date)
       ("perl-http-message" ,perl-http-message)
       ("perl-libwww" ,perl-libwww)
       ("perl-try-tiny" ,perl-try-tiny)
       ("perl-uri" ,perl-uri)))
    (home-page "https://myrepos.branchable.com/")
    (synopsis "Multiple repository management tool")
    (description
     "Myrepos provides the @code{mr} command, which maps an operation (e.g.,
fetching updates) over a collection of version control repositories.  It
supports a large number of version control systems: Git, Subversion,
Mercurial, Bazaar, Darcs, CVS, Fossil, and Veracity.")
    (license license:gpl2+)))

(define-public grokmirror
  (package
    (name "grokmirror")
    (version "2.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://git.kernel.org/pub/scm/"
                                 "utils/grokmirror/grokmirror.git"))
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "0zfiwjw02df3mzpawp9jx61iwp0nhcf6y03cs8022l0hkvc7blbr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-manpages
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((man (string-append (assoc-ref outputs "out")
                                        "/man/man1/")))
               (mkdir-p man)
               (for-each (lambda (file) (install-file file man))
                         (find-files "." "\\.1$")))
             #t)))))
    (propagated-inputs
     `(("python-packaging" ,python-packaging)
       ("python-requests" ,python-requests)))
    (home-page
     "https://git.kernel.org/pub/scm/utils/grokmirror/grokmirror.git")
    (synopsis "Framework to smartly mirror git repositories")
    (description "Grokmirror enables replicating large git repository
collections efficiently.  Mirrors decide to clone and update repositories
based on a manifest file published by servers.")
    (license license:gpl3+)))

(define-public b4
  (package
    (name "b4")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "b4" version))
       (sha256
        (base32 "1j904dy9cwxl85k2ngc498q5cdnqwsmw3jibjr1m55w8aqdck68z"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Fixes issue with dependency requirements being too strict. See upstream commit:
           ;; https://git.kernel.org/pub/scm/utils/b4/b4.git/commit/?id=31348a14afdb1d39e7faf9576eaddea1ced76e19
           (substitute* "setup.py"
             (("~=") ">="))
           #t))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ; No tests.
    (inputs
     `(("python-dkimpy" ,python-dkimpy)
       ("python-dnspython" ,python-dnspython)
       ("python-requests" ,python-requests)))
    (home-page "https://git.kernel.org/pub/scm/utils/b4/b4.git")
    (synopsis "Tool for working with patches in public-inbox archives")
    (description
     "The @code{b4} command is designed to make it easier to participate in
patch-based workflows for projects that have public-inbox archives.

Features include:
@itemize
@item downloading a thread's mbox given a message ID
@item processing an mbox so that is ready to be fed to @code{git-am}
@item creating templated replies for processed patches and pull requests
@item submitting cryptographic attestation for patches.
@end itemize")
    (license license:gpl2+)))

(define-public git-annex-remote-rclone
  (package
    (name "git-annex-remote-rclone")
    (version "0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DanielDent/git-annex-remote-rclone")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0j0hlxji8d974fq7zd4xc02n0jpi31ylhxc7z4zp8iiwad5mkpxp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((bash (string-append (assoc-ref %build-inputs "bash")
                                    "/bin/bash"))
               (rclone (string-append (assoc-ref %build-inputs "rclone")
                                      "/bin/rclone")))
           (copy-file (string-append (assoc-ref %build-inputs "source")
                                     "/git-annex-remote-rclone")
                      "git-annex-remote-rclone")
           (substitute* "git-annex-remote-rclone"
             (("/bin/bash") bash)
             (("runcmd rclone") (string-append "runcmd " rclone)))
           (install-file "git-annex-remote-rclone"
                         (string-append %output "/bin"))
           #t))))
    (inputs
     `(("bash" ,bash)
       ("rclone" ,rclone)))
    (home-page "https://github.com/DanielDent/git-annex-remote-rclone")
    (synopsis "Use rclone-supported cloud storage providers with git-annex")
    (description "This wrapper around rclone makes any destination supported
by rclone usable with git-annex.")
    (license license:gpl3+)))

(define-public fossil
  (package
    (name "fossil")
    (version "2.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
              "https://www.fossil-scm.org/index.html/uv/"
              "fossil-src-" version ".tar.gz"))
       (sha256
        (base32 "0c9nzx42wxfmym9vf1pnbdb1c7gp7a7zqky60izxsph7w2xh8nix"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "compat") #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("tcl" ,tcl)                     ;for configuration only
       ("which" ,which)                 ;for tests only
       ("ed" ,ed)))                     ;ditto
    (inputs
     `(("openssl" ,openssl)
       ("zlib" ,zlib)
       ("sqlite" ,sqlite)))
    (arguments
     `(#:configure-flags (list "--with-openssl=auto"
                               "--disable-internal-sqlite")
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  (add-after 'patch-source-shebangs 'patch-sh
                    (lambda _
                      (substitute* '("auto.def")
                        (("/bin/sh") (which "sh")))
                      #t))
                  (replace 'configure
                    (lambda* (#:key outputs (configure-flags '())
                              #:allow-other-keys)
                      ;; The 'configure' script is not an autoconf script and
                      ;; chokes on unrecognized options.
                      (apply invoke
                             "./configure"
                             (string-append "--prefix="
                                            (assoc-ref outputs "out"))
                             configure-flags)
                      #t))
                  (add-before 'check 'test-setup
                    (lambda _
                      (setenv "USER" "guix")
                      (setenv "TZ" "UTC")
                      #t)))))
    (home-page "https://fossil-scm.org")
    (synopsis "Software configuration management system")
    (description
     "Fossil is a distributed source control management system which supports
access and administration over HTTP CGI or via a built-in HTTP server.  It has
a built-in wiki, built-in file browsing, built-in tickets system, etc.")
    (license (list license:public-domain        ;src/miniz.c, src/shell.c
                   license:bsd-2))))

(define-public stagit
  (package
    (name "stagit")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.2f30.org/releases/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m3s9g1z9szbjrhm8sic91xh6f2bfpi56rskdkqd5wc4wdycpyi5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No configure script
    (inputs
     `(("libgit2" ,libgit2)))
    (home-page "https://2f30.org/")
    (synopsis "Static git page generator")
    (description "Stagit creates static pages for git repositories, the results can
be served with a HTTP file server of your choice.")
    (license license:expat)))

(define-public gource
  (package
    (name "gource")
    (version "0.51")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/acaudwell/Gource/releases/download"
                    "/gource-" version "/gource-" version ".tar.gz"))
              (sha256
               (base32
                "16p7b1x4r0915w883lp374jcdqqja37fnb7m8vnsfnl2n64gi8qr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost-libdir="
                            (assoc-ref %build-inputs "boost")
                            "/lib"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("boost"     ,boost)
       ("ftgl"      ,ftgl)
       ("glew"      ,glew)
       ("glm"       ,glm)
       ("glu"       ,glu)
       ("libpng"    ,libpng)
       ("mesa"      ,mesa)
       ("pcre"      ,pcre)
       ("sdl-union" ,(sdl-union (list sdl2 sdl2-image)))))
    (home-page "https://gource.io/")
    (synopsis "3D visualisation tool for source control repositories")
    (description "@code{gource} provides a software version control
visualization.  The repository is displayed as a tree where the root of the
repository is the centre, directories are branches and files are leaves.
Contributors to the source code appear and disappear as they contribute to
specific files and directories.")
    (license license:gpl3+)))

(define-public src
  (package
    (name "src")
    (version "1.18")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.catb.org/~esr/src/src-" version ".tar.gz"))
              (sha256
               (base32
                "0n0skhvya8w2az45h2gsafxy8m2mvqas64nrgxifcmrzfv0rf26c"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no 'configure' script
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (prog (string-append out "/bin/src"))
                    (rcs  (assoc-ref inputs "rcs")))
               (wrap-program prog
                 `("PATH" ":" prefix (,(string-append rcs "/bin"))))
               #t)))
         (replace 'check
           (lambda _
             (setenv "HOME" (getenv "TMPDIR"))
             (invoke "git" "config" "--global" "user.name" "guix")
             (invoke "git" "config" "--global" "user.email" "guix")
             (invoke "./srctest"))))))
    (native-inputs
     ;; For testing.
     `(("git" ,git)
       ("perl" ,perl)))
    (inputs
     `(("python" ,python-wrapper)
       ("rcs" ,rcs)))
    (synopsis "Simple revision control")
    (home-page "http://www.catb.org/~esr/src/")
    (description
     "SRC (or src) is simple revision control, a version-control system for
single-file projects by solo developers and authors.  It modernizes the
venerable RCS, hence the anagrammatic acronym.  The design is tuned for use
cases like all those little scripts in your @file{~/bin} directory, or a
directory full of HOWTOs.")
    (license license:bsd-2)))

(define-public git-when-merged
  ;; Use an unreleased version to get a PY3 compatibility fix.
  (let ((commit "ab6af7865a0ba55ba364a6c507e0be6f84f31c6d"))
    (package
      (name "git-when-merged")
      (version (string-append "1.2.0-" (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/mhagger/git-when-merged/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0iyk2psf97bc9h43m89p3xjmm79fsx99i7px29g4lcnmdy5kmz0p"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; there are no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'build)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "bin/git-when-merged"
                             (string-append (assoc-ref outputs "out")
                                            "/bin"))
               #t))
           (add-before 'install 'patch-git
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((git (string-append (assoc-ref inputs "git")
                                         "/bin/git")))
                 (substitute* "bin/git-when-merged"
                   (("'git'") (string-append "'" git "'")))
                 #t)))
           (add-after 'install 'wrap-script
             (lambda* (#:key outputs #:allow-other-keys)
               (wrap-program (string-append (assoc-ref outputs "out")
                                            "/bin/git-when-merged")
                 `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH"))))
               #t)))))
      (inputs
       `(("git" ,git)
         ("python" ,python-wrapper)))
      (home-page "https://github.com/mhagger/git-when-merged")
      (synopsis "Determine when a commit was merged into a Git branch")
      (description "This Git extension defines a subcommand,
@code{when-merged}, whose core operation is to find the merge that brought a
given commit into the specified ref(s).  It has various options that control
how information about the merge is displayed.")
      (license license:gpl2+))))

(define-public git-imerge
  (package
    (name "git-imerge")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mhagger/git-imerge")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vi1w3f0yk4gqhxj2hzqafqq28rihyhyfnp8x7xzib96j2si14a4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; only manual test scripts
       #:make-flags (list (string-append "DESTDIR=" %output)
                          "PREFIX=")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'patch-git
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((git (string-append (assoc-ref inputs "git")
                                       "/bin/git")))
               (substitute* "git-imerge"
                 (("'git'") (string-append "'" git "'")))
               #t)))
         (add-after 'install 'wrap-script
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/git-imerge")
               `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH"))))
             #t)))))
    (inputs
     `(("git" ,git)
       ("python" ,python-wrapper)))
    (home-page "https://github.com/mhagger/git-imerge")
    (synopsis "Incremental merge for Git")
    (description "This Git extension defines a subcommand, @code{imerge},
which performs an incremental merge between two branches.  Its two primary
design goals are to reduce the pain of resolving merge conflicts by finding
the smallest possible conflicts and to allow a merge to be saved, tested,
interrupted, published, and collaborated on while in progress.")
    (license license:gpl2+)))

(define-public git-lfs
  (package
    (name "git-lfs")
    (version "2.13.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/git-lfs/git-lfs")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r7dmqhkhz91d3n7qfpny483x8f1n88yya22j2fvx75rgg33z2sg"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/git-lfs/git-lfs"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'man-gen
           ;; Without this, the binary generated in 'build
           ;; phase won't have any embedded usage-text.
           (lambda _
             (with-directory-excursion "src/github.com/git-lfs/git-lfs"
               (invoke "make" "mangen"))))
         (add-after 'build 'build-man-pages
           (lambda _
             (with-directory-excursion "src/github.com/git-lfs/git-lfs"
               (invoke "make" "man"))
             #t))
         (add-after 'install 'install-man-pages
           (lambda _
             (with-directory-excursion "src/github.com/git-lfs/git-lfs/man"
               (let ((out (assoc-ref %outputs "out")))
                 (for-each
                   (lambda (manpage)
                     (install-file manpage (string-append out "/share/man/man1")))
                   (find-files "." "^git-lfs.*\\.1$"))))
             #t)))))
    ;; make `ronn` available during build for man page generation
    (native-inputs `(("ronn-ng" ,ronn-ng)))
    (home-page "https://git-lfs.github.com/")
    (synopsis "Git extension for versioning large files")
    (description
     "Git Large File Storage (LFS) replaces large files such as audio samples,
videos, datasets, and graphics with text pointers inside Git, while storing the
file contents on a remote server.")
    (license license:expat)))

(define-public git-open
  (package
    (name "git-open")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/paulirish/git-open")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11n46bngvca5wbdbfcxzjhjbfdbad7sgf7h9gf956cb1q8swsdm0"))))
    (build-system copy-build-system)
    (inputs
     `(("xdg-utils" ,xdg-utils)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (xdg-utils (assoc-ref inputs "xdg-utils")))
               (wrap-program (string-append out "/bin/git-open")
                 `("PATH" ":" prefix (,(string-append xdg-utils "/bin"))))))))
       #:install-plan
       '(("git-open" "bin/git-open"))))
    (home-page "https://github.com/paulirish/git-open")
    (synopsis "Open a Git repository's homepage from the command-line")
    (description
     "@code{git open} opens the repository's website from the command-line,
guessing the URL pattern from the @code{origin} remote.")
    (license license:expat)))

(define-public tla
  (package
    (name "gnu-arch")
    (version "1.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.gnu.org/old-gnu/gnu-arch/"
                                  "tla-" version ".tar.gz"))
              (sha256
               (base32
                "01mfzj1i6p4s8191cgd5850hds1zls88hkf9rb6qx1vqjv585aj0"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; In tar 1.32, '--preserve' is ambiguous and leads to an
                  ;; error, so address that.
                  (substitute* "src/tla/libarch/archive.c"
                    (("\"--preserve\"")
                     "\"--preserve-permissions\""))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (chdir "src")

                        (mkdir "=build")
                        (chdir "=build")

                        ;; For libneon's 'configure' script.
                        ;; XXX: There's a bundled copy of neon.
                        (setenv "CONFIG_SHELL" (which "sh"))

                        (invoke "../configure" "--prefix" out
                                "--config-shell" (which "sh")
                                "--with-posix-shell" (which "sh")
                                "--with-cc" "gcc")))))


       ;; There are build failures when building in parallel.
       #:parallel-build? #f
       #:parallel-tests? #f

       #:test-target "test"))
    (native-inputs
     `(("which" ,which)))
    (synopsis "Historical distributed version-control system")
    (description
     "GNU Arch, aka. @code{tla}, was one of the first free distributed
version-control systems (DVCS).  It saw its last release in 2006.  This
package is provided for users who need to recover @code{tla} repositories and
for historians.")
    (home-page "https://www.gnu.org/software/gnu-arch/")
    (license license:gpl2)))                      ;version 2 only

(define-public diff-so-fancy
  (package
    (name "diff-so-fancy")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/so-fancy/diff-so-fancy")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0aavxahzha2mms4vdwysk79pa6wzswpfwgsq2hwaxnaf66maahfl"))))
    (inputs
     `(("perl" ,perl)
       ("ncurses" ,ncurses)))
    (build-system copy-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-lib-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
               (substitute* "diff-so-fancy"
                 (("use lib.*$")
                  (string-append "use lib '" lib "';\n")))
               #t)))
         (add-after 'install 'symlink-executable
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (ncurses (assoc-ref inputs "ncurses"))
                   (perl (assoc-ref inputs "perl")))
               (wrap-program (string-append out "/bin/diff-so-fancy")
                 `("PATH" ":" prefix (,(string-append ncurses "/bin")
                                      ,(string-append perl "/bin"))))
               #t))))
       #:install-plan
       '(("lib" "lib")
         ("diff-so-fancy" "bin/"))))
    (home-page "https://github.com/so-fancy/diff-so-fancy")
    (synopsis "Makes diffs more human friendly and readable")
    (description
     "@code{diff-so-fancy} strives to make your diffs human readable instead
of machine readable.  This helps improve code quality and helps you spot
defects faster.")
    (license license:expat)))

(define-public go-github-go-git
  (package
    (name "go-github-go-git")
    (version "5.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-git/go-git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vkcmhh2qq8c38sjbnzf0wvg2rzr19wssaq177bsvrjwj1xz1qbs"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f ;requires network connection
       #:import-path "github.com/go-git/go-git/v5"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'setup
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((git (assoc-ref inputs "git"))
                    (git-bin (string-append (assoc-ref inputs "git") "/bin"))
                    (git-exe (string-append git-bin "/git")))
               (setenv "GIT_DIST_PATH=" git)
               (setenv "GIT_EXEC_PATH=" git-bin)
               (setenv "HOME" (getcwd))
               (invoke git-exe "config" "--global" "user.email" "gha@example.com")
               (invoke git-exe "config" "--global" "user.name" "GitHub Actions")
               #t)
             #t)))))
    (native-inputs
     `(("go-github-com-emirpasic-gods" ,go-github-com-emirpasic-gods)
       ("go-github-com-go-git-gcfg" ,go-github-com-go-git-gcfg)
       ("go-github-com-go-git-go-billy" ,go-github-com-go-git-go-billy)
       ("go-github-com-imdario-mergo" ,go-github-com-imdario-mergo)
       ("go-github-com-jbenet-go-context" ,go-github-com-jbenet-go-context)
       ("go-github-com-kevinburke-ssh-config" ,go-github-com-kevinburke-ssh-config)
       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
       ("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff)
       ("go-github-com-xanzy-ssh-agentf" ,go-github-com-xanzy-ssh-agent)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-gopkg-in-warnings" ,go-gopkg-in-warnings)
       ("go-github-com-go-git-go-git-fixtures" ,go-github-com-go-git-go-git-fixtures)
       ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
       ("go-github-com-alcortesm-tgz" ,go-github-com-alcortesm-tgz)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("git" ,git)))
    (home-page "https://github.com/go-git/")
    (synopsis "Git implementation library")
    (description "This package provides a Git implementation library.")
    (license license:asl2.0)))

(define-public gita
  (let ((commit "e41b504dca90a25e9be27f296da7ce22e5782893")
        (revision "1"))
    (package
      (name "gita")
      (version (git-version "0.12.9" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nosarthur/gita")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1k03zgcbhl91cgyh4k7ywyjp00y63q4bqbimncqh5b3lni8l8j5l"))))
      (build-system python-build-system)
      (native-inputs
       `(("git" ,git) ;for tests
         ("python-pytest" ,python-pytest)))
      (propagated-inputs
       `(("python-pyyaml" ,python-pyyaml)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "tests/test_main.py"
                 (("'gita\\\\n'") "'source\\n'")
                 (("'gita'") "'source'"))
               (invoke (string-append (assoc-ref inputs "git") "/bin/git")
                       "init")
               (add-installed-pythonpath inputs outputs)
               (invoke (string-append (assoc-ref inputs "python-pytest")
                                      "/bin/pytest")
                       "-vv" "tests")))
           (add-after 'install 'install-shell-completions
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bash-completion (string-append out "/etc/bash_completion.d"))
                      (zsh-completion (string-append out "/etc/zsh/site-functions")))
                 (mkdir-p bash-completion)
                 (copy-file ".gita-completion.bash"
                            (string-append bash-completion "/gita"))
                 (mkdir-p zsh-completion)
                 (copy-file ".gita-completion.zsh"
                            (string-append zsh-completion "/_gita"))))))))
      (home-page "https://github.com/nosarthur/gita")
      (synopsis "Command-line tool to manage multiple Git repos")
      (description "This package provides a command-line tool to manage
multiple Git repos.

This tool does two things:
@itemize
@item display the status of multiple Git repos such as branch, modification,
commit message side by side
@item (batch) delegate Git commands/aliases from any working directory
@end itemize

If several repos are related, it helps to see their status together.")
      (license license:expat))))

(define-public ghq
  (package
    (name "ghq")
    (version "1.1.7")
    (home-page "https://github.com/x-motemen/ghq")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "155sfmhmh4ia3iinm1s8fk7fxyn5dxdryad9xkbg7mr3i3ikqjwh"))))
    (build-system go-build-system)
    (arguments
     '(#:install-source? #f
       #:import-path "github.com/x-motemen/ghq"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs import-path #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bash-completion (string-append out "/etc/bash_completion.d"))
                    (zsh-completion (string-append out "/share/zsh/site-functions")))
               (with-directory-excursion (string-append "src/" import-path)
                 (mkdir-p bash-completion)
                 (copy-file "misc/bash/_ghq"
                            (string-append bash-completion "/ghq"))
                 (mkdir-p zsh-completion)
                 (copy-file "misc/zsh/_ghq"
                            (string-append zsh-completion "/_ghq"))))
             #t)))))
    (native-inputs
     `(("git" ,git-minimal)))
    (inputs
     `(("github.com/songmu/gitconfig" ,go-github-com-songmu-gitconfig)
       ("github.com/mattn/go-isatty" ,go-github-com-mattn-go-isatty)
       ("github.com/motemen/go-colorine" ,go-github-com-motemen-go-colorine)
       ("github.com/saracen/walker" ,go-github-com-saracen-walker)
       ("github.com/urfave/cli/v2" ,go-github-com-urfave-cli-v2)
       ("golang.org/x/net/html" ,go-golang-org-x-net-html)
       ("golang.org/x/sync/errgroup" ,go-golang.org-x-sync-errgroup)))
    (synopsis "Manage remote repository clones")
    (description
     "@code{ghq} provides a way to organize remote repository clones, like
@code{go get} does.  When you clone a remote repository by @code{ghq get}, ghq
makes a directory under a specific root directory (by default @file{~/ghq})
using the remote repository URL's host and path.")
    (license license:expat)))
