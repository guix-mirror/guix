;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2018 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2015, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2018 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 André <eu@euandre.org>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Jovany Leandro G.C <bit4bit@riseup.net>
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
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
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
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
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

(define-public git
  (package
   (name "git")
   ;; XXX When updating Git, check if the special 'git-source' input to cgit
   ;; needs to be updated as well.
   (version "2.21.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://kernel.org/software/scm/git/git-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0a0d0b07rmvs985zpndxxy0vzr0vq53kq5kyd68iv6gf8gkirjwc"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("native-perl" ,perl)
      ("gettext" ,gettext-minimal)
      ("git-manpages"
       ,(origin
          (method url-fetch)
          (uri (string-append
                "mirror://kernel.org/software/scm/git/git-manpages-"
                version ".tar.xz"))
          (sha256
           (base32
            "0lgcynqbjmfsvhfk14jvqyvb1xiyqgkgznb707vha38wjcjdqs1g"))))
      ;; For subtree documentation.
      ("asciidoc" ,asciidoc)
      ("docbook-xsl" ,docbook-xsl)
      ("xmlto" ,xmlto)))
   (inputs
    `(("curl" ,curl)
      ("expat" ,expat)
      ("openssl" ,openssl)
      ("perl" ,perl)
      ("python" ,python-2) ; CAVEAT: incompatible with python-3 according to INSTALL
      ("zlib" ,zlib)

      ;; Note: we keep this in inputs rather than native-inputs to work around
      ;; a problem in 'patch-shebangs'; see <https://bugs.gnu.org/31952>.
      ("bash-for-tests" ,bash)

      ;; For 'gitweb.cgi'
      ("perl-cgi" ,perl-cgi)

      ;; For 'git-svn'.
      ("subversion" ,subversion)
      ("perl-term-readkey" ,perl-term-readkey)

      ;; For 'git-send-email'
      ("perl-authen-sasl" ,perl-authen-sasl)
      ("perl-net-smtp-ssl" ,perl-net-smtp-ssl)
      ("perl-io-socket-ssl" ,perl-io-socket-ssl)

      ;; For 'git gui', 'gitk', and 'git citool'.
      ("tcl" ,tcl)
      ("tk" ,tk)))
   (outputs '("out"                               ; the core
              "send-email"                        ; for git-send-email
              "svn"                               ; git-svn
              "credential-netrc"                  ; git-credential-netrc
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

                     ;; By default 'make install' creates hard links for
                     ;; things in 'libexec/git-core', which leads to huge
                     ;; nars; see <https://bugs.gnu.org/21949>.
                     "NO_INSTALL_HARDLINKS=indeed")

      ;; Make sure the full bash does not end up in the final closure.
      #:disallowed-references (,bash)

      #:test-target "test"

      ;; Tests fail randomly when parallel: <https://bugs.gnu.org/29512>.
      #:parallel-tests? #f

      ;; The explicit --with-tcltk forces the build system to hardcode the
      ;; absolute file name to 'wish'.
      #:configure-flags (list (string-append "--with-tcltk="
                                             (assoc-ref %build-inputs "tk")
                                             "/bin/wish8.6")) ; XXX

      #:modules ((srfi srfi-1)
                 (srfi srfi-26)
                 ,@%gnu-build-system-modules)
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'modify-PATH
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((path (string-split (getenv "PATH") #\:))
                  (bash-full (assoc-ref inputs "bash-for-tests")))
              ;; Drop the test bash from PATH so that (which "sh") and
              ;; similar does the right thing.
              (setenv "PATH" (string-join
                              (remove (cut string-prefix? bash-full <>) path)
                              ":"))
              #t)))
        (add-after 'configure 'patch-makefiles
          (lambda _
            (substitute* "Makefile"
              (("/usr/bin/perl") (which "perl"))
              (("/usr/bin/python") (which "python")))
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
                (("\\$\\(XMLTO\\) -m \\$\\(MANPAGE_XSL\\) man")
                 (string-append "$(XMLTO) -x "
                                (string-append (assoc-ref inputs "docbook-xsl")
                                               "/xml/xsl/docbook-xsl-"
                                               ,(package-version docbook-xsl))
                                "/manpages/docbook.xsl -m $(MANPAGE_XSL) man")))
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
              (install-file "contrib/credential/netrc/git-credential-netrc"
                            (string-append netrc "/bin"))
              ;; Previously, Git.pm was automatically found by netrc.
              ;; Perl 5.26 changed how it locates modules so that @INC no
              ;; longer includes the current working directory (the Perl
              ;; community calls this "dotless @INC").
              (wrap-program (string-append netrc "/bin/git-credential-netrc")
                `("PERL5LIB" ":" prefix
                  (,(string-append (assoc-ref outputs "out") "/share/perl5"))))
              #t)))
        (add-after 'install 'install-subtree
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((subtree (assoc-ref outputs "subtree")))
              (install-file "contrib/subtree/git-subtree"
                            (string-append subtree "/bin"))
              (install-file "contrib/subtree/git-subtree.1"
                            (string-append subtree "/share/man/man1"))
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
           (add-before 'check 'delete-svn-test
             (lambda _
               ;; This test cannot run since we are not building 'git-svn'.
               (delete-file "t/t9020-remote-svn.sh")
               #t))
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
       ((#:configure-flags flags)
        ''())
       ((#:disallowed-references lst '())
        `(,perl ,@lst))))
    (outputs '("out"))
    (native-inputs
     `(("native-perl" ,perl)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("curl" ,curl)                             ;for HTTP(S) access
       ("expat" ,expat)                           ;for 'git push' over HTTP(S)
       ("openssl" ,openssl)
       ("perl" ,perl)
       ("zlib" ,zlib)
       ("bash-for-tests" ,bash)))))

(define-public libgit2
  (package
    (name "libgit2")
    (version "0.28.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libgit2/libgit2/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0swk2dyq5a4p1jn5wvbcsrxckhh808vifxz5y8w663avg541188c"))
              (patches (search-patches "libgit2-avoid-python.patch"
                                       "libgit2-mtime-0.patch"))

              ;; Remove bundled software.
              (snippet '(begin
                          (delete-file-recursively "deps")
                          #t))
              (modules '((guix build utils)))))
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags '("-DUSE_SHA1DC=ON") ; SHA-1 collision detection
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
         ;; Run checks more verbosely.
         (replace 'check
           (lambda _ (invoke "./libgit2_clar" "-v" "-Q"))))))
    (inputs
     `(("libssh2" ,libssh2)
       ("http-parser" ,http-parser)))
    (native-inputs
     `(("guile" ,guile-2.2)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; These two libraries are in 'Requires.private' in libgit2.pc.
     `(("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://libgit2.github.com/")
    (synopsis "Library providing Git core methods")
    (description
     "Libgit2 is a portable, pure C implementation of the Git core methods
provided as a re-entrant linkable library with a solid API, allowing you to
write native speed custom Git applications in any language with bindings.")
    ;; GPLv2 with linking exception
    (license license:gpl2)))

(define-public git-crypt
  (package
    (name "git-crypt")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/AGWA/git-crypt"
                                  "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0454fdmgm5f3razkn8n03lfqm5zyzvr4r2528zmlxiwba9518l2i"))))
    (build-system gnu-build-system)
    (inputs
     `(("git" ,git)
       ("openssl" ,openssl)))
    (arguments
     `(#:tests? #f ; No tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "make")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make" "install"
                       (string-append "PREFIX=" out))))))))
    (home-page "https://www.agwa.name/projects/git-crypt")
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
   (version "1.0.3")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://git.spwhitton.name/git-remote-gcrypt")
                   (commit version)))
             (file-name (string-append name "-" version "-checkout"))
             (sha256
              (base32
               "1vay3204729c7wajgn3nxf0s0hzwpdrw14pl6kd8w2ss25gvw2k1"))))
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
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://git.zx2c4.com/cgit/snapshot/cgit-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1gw2j5xc5qdx2hwiwkr8h6kgya7v9d9ff9j32ga1dys0cca7qm1w"))))
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
           ;; XXX CGit is currently incompatible with Git > 2.18.
           (uri "mirror://kernel.org/software/scm/git/git-2.18.0.tar.xz")
           (sha256
            (base32
             "14hfwfkrci829a9316hnvkglnqqw1p03cw9k56p4fcb078wbwh4b"))))
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

(define-public python-ghp-import
  (package
    (name "python-ghp-import")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/davisp/ghp-import/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0x887v690xsac2hzjkpbvp3a6crh3m08mqbk3nb4xwc9dnk869q7"))))
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
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gitdb2" version))
              (sha256
               (base32
                "0i608q9c47rdsmyac1cn6s0hzwwj7cb957y8fc9wacc5lnw8ak5v"))))
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
     `(("python-smmap2" ,python-smmap2)))
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

(define-public python2-gitdb
  (package-with-python2 python-gitdb))

(define-public python-gitpython
  (package
    (name "python-gitpython")
    (version "2.1.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "GitPython" version))
              (sha256
               (base32
                "1a357c28dnhgvq3saia7v29r71ynp48l2qp5xsmnc4vgzmdxqdw2"))))
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

(define-public python2-gitpython
  (package-with-python2 python-gitpython))

(define-public shflags
  (package
    (name "shflags")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/kward/shflags.git")
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
    (version "0.4.2-pre")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nvie/gitflow/")
                    (commit "15aab26490facf285acef56cb5d61025eacb3a69")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01fs97q76fdfnvmrh2cyjhywcs3pykf1dg58sy0frflnsdzs6prx"))))
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
    (home-page "http://nvie.com/posts/a-successful-git-branching-model/")
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
    (version "0.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ctmarinas/stgit.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ydgg744m671nkhg7h4q2z3b9vpbc9914rbc0wcgimqfqsxkxx2y"))))
    (build-system python-build-system)
    (inputs
     `(("git" ,git)))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Two tests will fail -> disable them. TODO: fix the failing tests
             (delete-file "t/t3300-edit.sh")
             (delete-file "t/t7504-commit-msg-hook.sh")
             (invoke "make" "test"))))))
    (home-page "http://procode.org/stgit/")
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
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/RichiH/vcsh/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ym3swkh738c3vciffvlr96vqzhwmzkb8ajqzap8f0j9n039a1mf"))))
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
      (home-page "http://dustin.sallings.org/2010/03/28/git-test-sequence.html")
      (synopsis "Run a command over a sequence of commits")
      (description
       "git-test-sequence is similar to an automated git bisect except it’s
linear.  It will test every change between two points in the DAG.  It will
also walk each side of a merge and test those changes individually.")
      (license (license:x11-style "file://LICENSE")))))

(define-public gitolite
  (package
    (name "gitolite")
    (version "3.6.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/sitaramc/gitolite/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1idxipg0df80bhjcxgwxs3lllqnkvhwpinmfv1xvg1l98fxiapgp"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
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
    (home-page "http://gitolite.com")
    (synopsis "Git access control layer")
    (description
     "Gitolite is an access control layer on top of Git, providing fine access
control to Git repositories.")
    (license license:gpl2)))

(define-public mercurial
  (package
    (name "mercurial")
    (version "4.7.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://www.mercurial-scm.org/"
                                 "release/mercurial-" version ".tar.gz"))
             (sha256
              (base32
               "1yq9r8s9jzj8hk2yizjk25s4w16yx9b8mbdj6wp8ld7j2r15kw4p"))))
    (build-system python-build-system)
    (arguments
     `(;; Restrict to Python 2, as Python 3 would require
       ;; the argument --c2to3.
       #:python ,python-2
       ;; FIXME: Disabled tests because they require the nose unit
       ;; testing framework: https://nose.readthedocs.org/en/latest/ .
       #:tests? #f))
    (home-page "https://www.mercurial-scm.org/")
    (synopsis "Decentralized version control system")
    (description
     "Mercurial is a free, distributed source control management tool.
It efficiently handles projects of any size
and offers an easy and intuitive interface.")
    (license license:gpl2+)))

(define-public neon
  (package
    (name "neon")
    (version "0.30.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.webdav.org/neon/neon-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1jpvczcx658vimqm7c8my2q41fnmjaf1j03g7bsli6rjxk6xh2yv"))))
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
                           ;; requires libgnutils-config, deprecated
                           ;; in gnutls 2.8.
                           ; "--with-ssl=gnutls")))
                           "--with-ssl=openssl")))
    (home-page "http://www.webdav.org/neon/")
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
    (version "1.10.2")
    (source (origin
             (method url-fetch)
             (uri
               (list
                 (string-append "https://archive.apache.org/dist/subversion/"
                                "subversion-" version ".tar.bz2")
                 (string-append "https://www-eu.apache.org/dist/subversion/"
                                "subversion-" version ".tar.bz2")))
             (sha256
              (base32
               "127dysfc31q4dhbbxaznh9kqixy9jd44kgwji2gdwj6rb2lf6dav"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
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
    (version "5.9.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/rcs/rcs-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1zsx7bb0rgvvvisiy4zlixf56ay8wbd9qqqcp1a1g0m1gl6mlg86"))
             (patches (search-patches "rcs-5.9.4-noreturn.patch"))))
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
             (patches (search-patches "cvs-2017-12836.patch"))
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
    (version "1.45")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.catb.org/~esr/cvs-fast-export/"
                                  "cvs-fast-export-" version ".tar.gz"))
              (sha256
               (base32
                "19pxg6p0pcgyd2fbnh3wy1kazv6vcfi5lzc2whhdi1w9kj4r9c4z"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'unpack 'remove-optimizations
           (lambda _
             ;; Don't optimize for a specific processor architecture.
             (substitute* "Makefile"
               (("CFLAGS \\+= -march=native") ""))
             #t)))
       #:parallel-build? #f ; parallel a2x commands fail spectacularly
       #:make-flags
       (list "CC=gcc" (string-append "prefix?=" (assoc-ref %outputs "out")))))
    (inputs `(("git" ,git)))
    (native-inputs `(("asciidoc"    ,asciidoc)
                     ;; These are needed for the tests.
                     ("cvs"    ,cvs)
                     ("python" ,python-2)
                     ("rcs"    ,rcs)))
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
    (version "1.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/vc-dwim/vc-dwim-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0d5sqafc40l878m8wjr35jxmalj4kam1m6ph60v08ng4ml5g7931"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)
              ("inetutils" ,inetutils))) ; for `hostname', used in the tests
    (native-inputs `(("emacs" ,emacs-minimal))) ; for `ctags'
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
    (version "1.62")
    (source (origin
              (method url-fetch)
              (uri
               (list
                 (string-append "ftp://invisible-island.net/diffstat/"
                                name "-" version ".tgz")
                 (string-append "http://invisible-mirror.net/archives/diffstat/"
                                name "-" version ".tgz")))
              (sha256
               (base32
                "07sr482y6iw7n7ddkba0w51kbjc99snvnijkn5ba2xzd8hv1h2bz"))))
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
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/" name "/CSSC-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15191dh8hr46cvssmv4v52gymiiyk6ca9j1bfimlqakcqab6y51h"))))
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
                               "--sharedstatedir=/var/com/aegis")
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
    (home-page "http://aegis.sourceforge.net")
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
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/jonas/tig/releases/download/tig-"
                    version "/tig-" version ".tar.gz"))
              (sha256
               (base32
                "1f2qhpzbl7f35lsjcnx8lxzskha24m4frczsw78284jp7qcamdmn"))))
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
       #:tests? #f)) ; tests require access to /dev/tty
    ;; #:test-target "test"))
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
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/0-wiz-0/findnewest/archive/findnewest-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1ydis4y0amkgfr4y60sn076f1l41ya2kn89kfd9fqf44f9ccgb5r"))))
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

(define-public git-annex-remote-hubic
  (package
    (name "git-annex-remote-hubic")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Schnouki/" name "/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "196g3jkaybjx11nbr51n0cjps3wjzb145ab76y717diqvvxp5v4r"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (native-inputs
     `(;; for the tests
       ("python2-six" ,python2-six)))
    (propagated-inputs
     `(("python2-dateutil" ,python2-dateutil)
       ("python2-futures" ,python2-futures)
       ("python2-rauth" ,python2-rauth)
       ("python2-swiftclient" ,python2-swiftclient)))
    (home-page "https://github.com/Schnouki/git-annex-remote-hubic/")
    (synopsis "Use hubic as a git-annex remote")
    (description
     "This package allows you to use your hubic account as a \"special
repository\" with git-annex.")
    (license license:gpl3+)))

(define-public fossil
  (package
    (name "fossil")
    (version "2.8")
    (source
     (origin
       (method url-fetch)
       ;; Older downloads are moved to another URL.
       (uri (list
             (string-append
              "https://www.fossil-scm.org/index.html/uv/download/"
              "fossil-src-" version ".tar.gz")
             (string-append
              "https://www.fossil-scm.org/index.html/uv/"
              "fossil-src-" version ".tar.gz")))
       (sha256
        (base32
         "0pbinf8d2kj1j7niblhzjd2l2khg6r2pn2xvig6gavz27p3vwcka"))
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
       ("sqlite" ,sqlite-3.26.0)))
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

;; Darcs has no https support: http://irclog.perlgeek.de/darcs/2016-09-17
;; http://darcs.net/manual/Configuring_darcs.html#SECTION00440070000000000000
;; and results of search engines will show that if the protocol is http, https
;; is never mentioned.
(define-public darcs
  (package
    (name "darcs")
    (version "2.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/darcs/"
                           "darcs-" version ".tar.gz"))
       (sha256
        (base32
         "0zm2486gyhiga1amclbg92cd09bvki6vgh0ll75hv5kl72j61lb5"))
       (modules '((guix build utils)))
       ;; Remove time-dependent code for reproducibility.
       (snippet
        '(begin
           (substitute* "darcs/darcs.hs"
             (("__DATE__") "\"1970-01-01\"")
             (("__TIME__") "\"00:00:00\""))
           #t))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags '("-fpkgconfig" "-fcurl" "-flibiconv" "-fthreaded"
                           "-fnetwork-uri" "-fhttp" "--flag=executable"
                           "--flag=library")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-sh
           (lambda _
             (substitute* "tests/issue538.sh"
               (("/bin/sh") (which "sh")))
             #t)))))
    (inputs
     `(("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-split" ,ghc-split)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-findbin" ,ghc-findbin)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-async" ,ghc-async)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base16-bytestring" ,ghc-base16-bytestring)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-cryptohash" ,ghc-cryptohash)
       ("ghc-data-ordlist" ,ghc-data-ordlist)
       ("ghc-fgl" ,ghc-fgl)
       ("ghc-system-filepath" ,ghc-system-filepath)
       ("ghc-graphviz" ,ghc-graphviz)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-html" ,ghc-html)
       ("ghc-mmap" ,ghc-mmap)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-random" ,ghc-random)
       ("ghc-regex-applicative" ,ghc-regex-applicative)
       ("ghc-regex-compat-tdfa" ,ghc-regex-compat-tdfa)
       ("ghc-sandi" ,ghc-sandi)
       ("ghc-shelly" ,ghc-shelly)
       ("ghc-tar" ,ghc-tar)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-vector" ,ghc-vector)
       ("ghc-zip-archive" ,ghc-zip-archive)
       ("ghc-zlib" ,ghc-zlib)
       ("ghc-http" ,ghc-http)
       ("curl" ,curl)
       ("ghc" ,ghc)
       ("ncurses" ,ncurses)
       ("perl" ,perl)
       ("libiconv" ,libiconv)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://darcs.net")
    (synopsis "Distributed Revision Control System")
    (description
     "Darcs is a revision control system.  It is:

@enumerate
@item Distributed: Every user has access to the full command set, removing boundaries
between server and client or committer and non-committers.
@item Interactive: Darcs is easy to learn and efficient to use because it asks you
questions in response to simple commands, giving you choices in your work flow.
You can choose to record one change in a file, while ignoring another.  As you update
from upstream, you can review each patch name, even the full diff for interesting
patches.
@item Smart: Originally developed by physicist David Roundy, darcs is based on a
unique algebra of patches called @url{http://darcs.net/Theory,Patchtheory}.
@end enumerate")
    (license license:gpl2)))

(define-public java-jgit
  (package
    (name "java-jgit")
    (version "4.7.0.201704051617-r")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/jgit/org.eclipse.jgit/"
                                  version "/org.eclipse.jgit-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "13ii4jn02ynzq6i7gsyi21k2i94jpc85wf6bcm31q4cyvzv0mk4k"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f                      ; There are no tests to run.
       #:jar-name "jgit.jar"
       ;; JGit must be built with a JDK supporting Java 8.
       #:jdk ,icedtea-8
       ;; Target our older default JDK.
       #:make-flags (list "-Dtarget=1.7")
       #:phases
       (modify-phases %standard-phases
         ;; The jar file generated by the default build.xml does not include
         ;; the text properties files, so we need to add them.
         (add-after 'build 'add-properties
           (lambda* (#:key jar-name #:allow-other-keys)
             (with-directory-excursion "src"
               (apply invoke "jar" "-uf"
                      (string-append "../build/jar/" jar-name)
                      (find-files "." "\\.properties$")))
             #t)))))
    (inputs
     `(("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-javaewah" ,java-javaewah)
       ("java-jsch" ,java-jsch)
       ("java-slf4j-api" ,java-slf4j-api)))
    (home-page "https://eclipse.org/jgit/")
    (synopsis "Java library implementing the Git version control system")
    (description "JGit is a lightweight, pure Java library implementing the
Git version control system, providing repository access routines, support for
network protocols, and core version control algorithms.")
    (license license:edl1.0)))

;; For axoloti.  This package can still be built with icedtea-7, which is
;; currently used as the default JDK.
(define-public java-jgit-4.2
  (package (inherit java-jgit)
    (version "4.2.0.201601211800-r")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "org/eclipse/jgit/org.eclipse.jgit/"
                                  version "/org.eclipse.jgit-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "15gm537iivhnzlkjym4x3wn5jqdjdragsw9pdpzqqg21nrc817mm"))))
    (build-system ant-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments java-jgit)
       ;; Build for default JDK.
       ((#:jdk _) icedtea-7)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'use-latest-javaewah-API
             (lambda _
               (substitute* "src/org/eclipse/jgit/internal/storage/file/BitmapIndexImpl.java"
                 (("wordinbits") "WORD_IN_BITS"))
               #t))))))
    (inputs
     `(("java-javaewah" ,java-javaewah)
       ("java-jsch" ,java-jsch)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gource
  (package
    (name "gource")
    (version "0.49")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/acaudwell/Gource/releases/download"
                    "/gource-" version "/gource-" version ".tar.gz"))
              (sha256
               (base32
                "12hf5ipcsp9dxsqn84n4kr63xaiskrnf5a084wr29qk171lj7pd9"))))
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
    (home-page "http://gource.io/")
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

(define-public git-annex
  (package
    (name "git-annex")
    (version "6.20180926")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "git-annex/git-annex-" version ".tar.gz"))
       (sha256
        (base32
         "1251rj8h63y30sfqk0zh670yhz14p256y59n3590pg015pf3575d"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags
       '("--flags=-Android -Assistant -Pairing -S3 -Webapp -WebDAV")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-shell
           (lambda _
             (substitute* "Utility/Shell.hs"
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'configure 'factor-setup
           (lambda _
             ;; Factor out necessary build logic from the provided
             ;; `Setup.hs' script.  The script as-is does not work because
             ;; it cannot find its dependencies, and there is no obvious way
             ;; to tell it where to look.  Note that we do not preserve the
             ;; code that installs man pages here.
             (call-with-output-file "PreConf.hs"
               (lambda (out)
                 (format out "import qualified Build.Configure as Configure~%")
                 (format out "main = Configure.run Configure.tests~%")))
             (call-with-output-file "Setup.hs"
               (lambda (out)
                 (format out "import Distribution.Simple~%")
                 (format out "main = defaultMain~%")))
             #t))
         (add-before 'configure 'pre-configure
           (lambda _
             (invoke "runhaskell" "PreConf.hs")
             #t))
         (replace 'check
           (lambda _
             ;; We need to set the path so that Git recognizes
             ;; `git annex' as a custom command.
             (setenv "PATH" (string-append (getenv "PATH") ":"
                                           (getcwd) "/dist/build/git-annex"))
             (with-directory-excursion "dist/build/git-annex"
               (symlink "git-annex" "git-annex-shell"))
             (invoke "git-annex" "test")
             #t))
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-annex-shell"))
               (symlink (string-append bin "/git-annex")
                        (string-append bin "/git-remote-tor-annex"))
               #t))))))
    (inputs
     `(("curl" ,curl)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-async" ,ghc-async)
       ("ghc-bloomfilter" ,ghc-bloomfilter)
       ("ghc-byteable" ,ghc-byteable)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-crypto-api" ,ghc-crypto-api)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-disk-free-space" ,ghc-disk-free-space)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-edit-distance" ,ghc-edit-distance)
       ("ghc-esqueleto" ,ghc-esqueleto)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-feed" ,ghc-feed)
       ("ghc-free" ,ghc-free)
       ("ghc-hslogger" ,ghc-hslogger)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-conduit" ,ghc-http-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-ifelse" ,ghc-ifelse)
       ("ghc-memory" ,ghc-memory)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-network" ,ghc-network)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-persistent" ,ghc-persistent)
       ("ghc-persistent-sqlite" ,ghc-persistent-sqlite)
       ("ghc-persistent-template" ,ghc-persistent-template)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-random" ,ghc-random)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-safesemaphore" ,ghc-safesemaphore)
       ("ghc-sandi" ,ghc-sandi)
       ("ghc-securemem" ,ghc-securemem)
       ("ghc-socks" ,ghc-socks)
       ("ghc-split" ,ghc-split)
       ("ghc-stm" ,ghc-stm)
       ("ghc-stm-chans" ,ghc-stm-chans)
       ("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-text" ,ghc-text)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-uuid" ,ghc-uuid)
       ("git" ,git)
       ("rsync" ,rsync)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-rerun" ,ghc-tasty-rerun)))
    (home-page "https://git-annex.branchable.com/")
    (synopsis "Manage files with Git, without checking in their contents")
    (description "This package allows managing files with Git, without
checking the file contents into Git.  It can store files in many places,
such as local hard drives and cloud storage services.  It can also be
used to keep a folder in sync between computers.")
    ;; The web app is released under the AGPLv3+.
    (license (list license:gpl3+
                   license:agpl3+))))

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
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/mhagger/git-imerge/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0a6ay8fpgz3yd84jc40w41x0rcfpan6bcq4wd1hxiiqwb51jysb2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; The are only manual test scripts.
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
