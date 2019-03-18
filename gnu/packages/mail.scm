;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2014 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2014 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Al McElrath <hello@yrns.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016, 2017, 2018 ng0 <ng0@n0.is>
;;; Copyright © 2016 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2016, 2017, 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2018 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages mail)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages django)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages search)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses)
                #:select (fdl1.1+
                           agpl3+
                           gpl2 gpl2+ gpl3 gpl3+ lgpl2.1 lgpl2.1+ lgpl3+
                           non-copyleft (expat . license:expat) bsd-3
                           public-domain bsd-4 isc (openssl . license:openssl)
                           bsd-2 x11-style agpl3 asl2.0 perl-license))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial))

(define-public mailutils
  (package
    (name "mailutils")
    (version "3.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/mailutils/mailutils-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1wx275w38fwni2abc8g7g3irbk332vr34byxd72zqfdiznsqgims"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'prepare-test-suite
           (lambda _
             ;; Use the right file name for `cat'.
             (substitute* "testsuite/lib/mailutils.exp"
               (("/bin/cat")
                (which "cat")))

             ;; Tests try to invoke 'maidag' such that it looks up the
             ;; 'root' user, which does not exist in the build
             ;; environment.
             (substitute* "maidag/tests/testsuite"
               (("root <")         "nobody <")
               (("spool/root")     "spool/nobody")
               (("root@localhost") "nobody@localhost"))

             ;; The 'pipeact.at' tests generate a shell script; make
             ;; sure it uses the right shell.
             (substitute* '("sieve/tests/testsuite"
                            "mh/tests/testsuite")
               (("#! /bin/sh")
                (string-append "#!" (which "sh"))))

             (substitute* "mh/tests/testsuite"
               (("moreproc: /bin/cat")
                (string-append "moreproc: " (which "cat"))))

             ;; XXX: The comsatd tests rely on being able to open
             ;; /dev/tty, but that gives ENODEV in the build
             ;; environment.  Thus, ignore test failures here.
             (substitute* "comsat/tests/Makefile.in"
               (("\\$\\(SHELL\\) \\$\\(TESTSUITE\\)" all)
                (string-append "-" all)))

             ;; 'frm' tests expect write access to $HOME.
             (setenv "HOME" (getcwd))

             ;; Avoid the message "I'm going to create the standard MH path
             ;; for you", which would lead to one test failure (when diffing
             ;; stdout of 'fmtcheck'.)
             (call-with-output-file ".mh_profile"
               (lambda (port)
                 (format port "Path: ~a/Mail-for-tests~%"
                         (getcwd))))

             #t)))
       ;; TODO: Add `--with-sql'.
       #:configure-flags (list "--sysconfdir=/etc"

                               ;; Add "/2.2" to the installation directory.
                               (string-append "--with-guile-site-dir="
                                              (assoc-ref %outputs "out")
                                              "/share/guile/site/2.2"))

       #:parallel-tests? #f))
    (native-inputs
     `(("perl" ,perl)))                           ;for 'gylwrap'
    (inputs
     `(("dejagnu" ,dejagnu)
       ("m4" ,m4)
       ("texinfo" ,texinfo)
       ("guile" ,guile-2.2)
       ("gsasl" ,gsasl)
       ("gnutls" ,gnutls)
       ("ncurses" ,ncurses)
       ("readline" ,readline)
       ("linux-pam" ,linux-pam)
       ("libltdl" ,libltdl)
       ("gdbm" ,gdbm)))
    (home-page "https://mailutils.org")
    (synopsis "Utilities and library for reading and serving mail")
    (description
     "GNU Mailutils is a collection of programs for managing, viewing and
processing electronic mail.  It contains both utilities and server daemons
and all operate in a protocol-agnostic way.  The underlying libraries are
also available, simplifying the addition of mail capabilities to new
software.")
    (license
     ;; Libraries are under LGPLv3+, and programs under GPLv3+.
     (list gpl3+ lgpl3+))))

(define-public nullmailer
  (package
    (name "nullmailer")
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (list
             (string-append "https://untroubled.org/nullmailer/"
                            "nullmailer-" version ".tar.gz")
             ;; Previous releases are moved to this subdirectory.
             (string-append "https://untroubled.org/nullmailer/archive/"
                            "nullmailer-" version ".tar.gz")))
       (sha256
        (base32 "0md8cf90fl2yf3zh9njjy42a673v4j4ygyq95xg7fzkygdigm1lq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-tls"
             "--localstatedir=/var"
             "--sysconfdir=/etc")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-test-FHS-file-names
           (lambda _
             (with-directory-excursion "test"
               (substitute* (list "functions.in"
                                  "tests/send")
                 ;; Fix some shebangs later generated on the fly.
                 (("/bin/sh") (which "bash"))))
             #t))
         (add-before 'check 'pass-PATH-to-tests
           ;; ‘runtest’ launches each test through ‘env -’, clearing $PATH. The
           ;; tests then source ‘functions’, which first demands a working $PATH
           ;; only to clobber it later.  Pass our $PATH to the test environment
           ;; and don't touch it after that.
           (lambda _
             (with-directory-excursion "test"
               (substitute* "runtests"
                 (("env - bash")
                  (string-append "env - PATH=\"" (getenv "PATH") "\" bash")))
               (substitute* "functions.in"
                 (("export PATH=.*") "")))
             #t))
         (add-before 'check 'delete-failing-tests
           (lambda _
             (with-directory-excursion "test/tests"
               (for-each delete-file
                         (list
                          ;; XXX ‘nullmailer-inject: nullmailer-queue failed: 15’
                          "inject/queue"
                          ;; XXX These require the not-yet-packaged tcpserver.
                          "protocols" "smtp-auth")))
             #t))
         (add-before 'install 'skip-install-data-local
           ;; Don't attempt to install run-time files outside of the store.
           (lambda _
             (substitute* "Makefile"
               ((" install-data-local") ""))
             #t)))))
    (native-inputs
     ;; For tests.
     `(("daemontools" ,daemontools)))   ; for svc
    (inputs
     `(("gnutls" ,gnutls)))
    (home-page "https://untroubled.org/nullmailer/")
    (synopsis "Simple relay-only mail transfer agent")
    (description
     "Nullmailer is a simple replacement @acronym{MTA, Mail Transfer Agent} for
hosts that receive no local mail and only relay mail to a fixed set of smart
relays.  It's useful for systems such as Web servers that must be able to send
email notifications, without having to run a full-blown MTA such as sendmail
or qmail.

Nullmailer is designed to be simple to configure, easy to extend, and secure.
It requires little ongoing administration.  The included @command{sendmail}
emulator front-end should allow most (if not all) sendmail-compatible programs
to run without any changes.")
    (license (list lgpl2.1+         ; lib/cli++/ (but some files lack headers)
                   gpl2+))))        ; everything else

(define-public fetchmail
  (package
    (name "fetchmail")
    (version "6.3.26")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/fetchmail/branch_6.3/fetchmail-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0l78ayvi9dm8hd190gl139cs2xqsrf7r9ncilslw20mgvd6cbd3r"))))
    (build-system gnu-build-system)
    (inputs
     `(("openssl" ,openssl)))
    (arguments
     `(#:configure-flags (list (string-append "--with-ssl="
                                              (assoc-ref %build-inputs "openssl")))))
    (home-page "http://www.fetchmail.info/")
    (synopsis "Remote-mail retrieval and forwarding utility")
    (description
     "Fetchmail is a full-featured, robust, well-documented remote-mail
retrieval and forwarding utility intended to be used over on-demand
TCP/IP links (such as SLIP or PPP connections).  It supports every
remote-mail protocol now in use on the Internet: POP2, POP3, RPOP, APOP,
KPOP, all flavors of IMAP, ETRN, and ODMR.  It can even support IPv6
and IPSEC.

Fetchmail retrieves mail from remote mail servers and forwards it via SMTP,
so it can then be read by normal mail user agents such as mutt, elm
or BSD Mail.  It allows all your system MTA's filtering, forwarding, and
aliasing facilities to work just as they would on normal mail.")
    (license gpl2+))) ; most files are actually public domain or x11

(define-public mutt
  (package
    (name "mutt")
    (version "1.11.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://bitbucket.org/mutt/mutt/downloads/"
                                 "mutt-" version ".tar.gz"))
             (sha256
              (base32
               "0h8rmcc62n1pagm7mjjccd5fxyhhi4vbvp8m88digkdf5z0g8hm5"))
             (patches (search-patches "mutt-store-references.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("gdbm" ,gdbm)
       ("gpgme" ,gpgme)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("perl" ,perl)))
    (arguments
     `(#:configure-flags '("--enable-smtp"
                           "--enable-imap"
                           "--enable-pop"
                           "--enable-gpgme"
                           "--enable-hcache" ; for header caching
                           "--enable-sidebar"
                           "--with-ssl"
                           "--with-sasl"
                           ;; so that mutt does not check whether the path
                           ;; exists, which it does not in the chroot
                           "--with-mailpath=/var/mail")))
    (home-page "http://www.mutt.org/")
    (synopsis "Mail client")
    (description
     "Mutt is a small but very powerful text-based mail client for Unix
operating systems.")
    (license gpl2+)))

(define-public neomutt
  (package
    (name "neomutt")
    (version "20180716")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/" name "/" name
                           "/archive/" name "-" version ".tar.gz"))
       (sha256
        (base32
         "0072in2d6znwqq461shsaxlf40r4zr7w3j9848qvm4xlh1lq52dx"))))
    (build-system gnu-build-system)
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("gdbm" ,gdbm)
       ("gpgme" ,gpgme)
       ("ncurses" ,ncurses)
       ("gnutls" ,gnutls)
       ("openssl" ,openssl) ;For smime
       ("perl" ,perl)
       ("kyotocabinet" ,kyotocabinet)
       ("libxslt" ,libxslt)
       ("libidn" ,libidn)
       ("libxml2" ,libxml2)
       ("lmdb" ,lmdb)
       ("notmuch" ,notmuch)))
    (native-inputs
     `(("automake" ,automake)
       ("gettext-minimal" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml-4.2)
       ("w3m" ,w3m)
       ("tcl" ,tcl)))
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list "--gpgme"

             ;; database, implies header caching
             "--disable-tokyocabinet"
             "--disable-qdbm"
             "--disable-bdb"
             "--lmdb"
             "--kyotocabinet"

             "--gdbm"

             "--gnutls"
             "--disable-ssl"
             "--sasl"
             (string-append "--with-sasl="
                            (assoc-ref %build-inputs "cyrus-sasl"))


             "--smime"
             "--notmuch"
             "--idn"

             ;; If we do not set this, neomutt wants to check
             ;; whether the path exists, which it does not
             ;; in the chroot. The workaround is this.
             "--with-mailpath=/var/mail"

             "--with-ui=ncurses"
             (string-append "--with-ncurses="
                            (assoc-ref %build-inputs "ncurses"))
             (string-append "--prefix="
                            (assoc-ref %outputs "out"))
             "--debug")
       #:phases
       (modify-phases %standard-phases
         ;; TODO: autosetup is meant to be included in the source,
         ;; but we should package autosetup and use our own version of it.
         (add-before 'configure 'fix-sasl-test
           (lambda _
             ;; Upstream suggestion to fix the failing sasl autosetup test.
             (substitute* "auto.def"
               (("cc-with \\[list -cflags -I\\$prefix/include -libs")
                "cc-with [list -includes stddef.h -cflags -I$prefix/include -libs"))
             #t))
         (replace 'configure
           (lambda* (#:key outputs inputs configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (flags `(,@configure-flags))
                    (bash (which "bash")))
               (setenv "SHELL" bash)
               (setenv "CONFIG_SHELL" bash)
               (apply invoke bash
                      (string-append (getcwd) "/configure")
                      flags)))))))
    (home-page "https://www.neomutt.org/")
    (synopsis "Command-line mail reader based on Mutt")
    (description
     "NeoMutt is a command-line mail reader which is based on mutt.
It adds a large amount of new and improved features to mutt.")
    (license gpl2+)))

(define-public gmime
  (package
    (name "gmime")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gmime/"
                                  (version-major+minor version)
                                  "/gmime-" version ".tar.xz"))
              (sha256
               (base32
                "1q6palbpf6lh6bvy9ly26q5apl5k0z0r4mvl6zzqh90rz4rn1v3m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gnupg" ,gnupg)))               ; for tests only
    (inputs `(("glib" ,glib)
              ("gpgme" ,gpgme)
              ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-paths-in-tests
          (lambda _
            ;; The test programs run several programs using 'system' with
            ;; hard-coded paths.  Here we patch them all.
            ;; We use ISO-8859-1 here because test-iconv.c contains
            ;; raw byte sequences in several different encodings.
            (with-fluids ((%default-port-encoding #f))
              (substitute* (find-files "tests" "\\.c$")
                (("(system *\\(\")(/[^ ]*)" all pre prog-path)
                 (let* ((base (basename prog-path))
                        (prog (which base)))
                   (string-append pre
                                  (or prog (error "not found: " base)))))))
            #t)))))
    (home-page "http://spruce.sourceforge.net/gmime/")
    (synopsis "MIME message parser and creator library")
    (description
     "GMime provides a core library and set of utilities which may be used for
the creation and parsing of messages using the Multipurpose Internet Mail
Extension (MIME).")
    (license (list lgpl2.1+ gpl2+ gpl3+))))

;; Some packages are not ready for GMime 3 yet.
(define-public gmime-2.6
  (package
    (inherit gmime)
    (version "2.6.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gmime/"
                                  (version-major+minor version)
                                  "/gmime-" version ".tar.xz"))
              (sha256
               (base32
                "0slzlzcr3h8jikpz5a5amqd0csqh2m40gdk910ws2hnaf5m6hjbi"))))))

(define-public bogofilter
  (package
    (name "bogofilter")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bogofilter/bogofilter-"
                                  version "/bogofilter-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1d56n2m9inm8gnzm88aa27xl2a7sp7aff3484vmflpqkinjqf0p1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (substitute* "src/tests/t.frame"
               (("GREP=/bin/grep")
                (string-append "GREP=" (which "grep") "\n")))
             #t)))))
    (native-inputs `(("flex" ,flex)))
    (inputs `(("bdb" ,bdb)))
    (home-page "http://bogofilter.sourceforge.net/")
    (synopsis "Mail classifier based on a Bayesian filter")
    (description
     "Bogofilter is a mail filter that classifies mail as spam or ham
 (non-spam) by a statistical analysis of the message's header and
content (body).  The program is able to learn from the user's classifications
and corrections.  It is based on a Bayesian filter.")
    (license gpl2)))

(define-public offlineimap
  (package
    (name "offlineimap")
    (version "7.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OfflineIMAP/offlineimap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18sdnhjldn8zs03bgqy1qa3ikmlfvyxcvwp3nbnv1a74biccqbpa"))))
    (build-system python-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)))
    (inputs `(("python2-pysqlite" ,python2-pysqlite)
              ("python2-six" ,python2-six)))
    (arguments
     ;; The setup.py script expects python-2.
     `(#:python ,python-2
      ;; Tests require a modifiable IMAP account.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-documentation
           (lambda _
             (substitute* "docs/Makefile"
               ;; Prevent xmllint and xsltproc from downloading a DTD file.
               (("a2x -v") "a2x --no-xmllint --xsltproc-opts=--nonet -v"))
             (invoke "make" "-C" "docs" "man")))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (install-file "docs/offlineimap.1" (string-append man "/man1"))
               (install-file "docs/offlineimapui.7" (string-append man "/man7"))
               #t))))))
    (home-page "https://www.offlineimap.org")
    (synopsis "Sync emails between two repositories")
    (description
     "OfflineImap synchronizes emails between two repositories, so that you
can read the same mailbox from multiple computers.  It supports IMAP as REMOTE
repository and Maildir/IMAP as LOCAL repository.")
    (license gpl2+)))

(define-public emacs-mew
  (package
    (name "emacs-mew")
    (version "6.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mew.org/Release/mew-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03fzky2kz73vgx4cbps2psbbnrgqgkk5q7jwfldisymkzr9iz03y"))))
    (native-inputs
     `(("emacs" ,emacs)))
    (propagated-inputs
     `(("ruby-sqlite3" ,ruby-sqlite3) ; optional for the database of messages
       ("ruby" ,ruby))) ; to set GEM_PATH so ruby-sqlite3 is found at runtime
    (build-system gnu-build-system)
    (arguments
     (let ((elisp-dir "/share/emacs/site-lisp/guix.d/mew")
           (icon-dir  "/share/mew"))
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (guix build emacs-utils))
         #:imported-modules (,@%gnu-build-system-modules
                             (guix build emacs-utils))
         #:configure-flags
         (list (string-append "--with-elispdir=" %output ,elisp-dir)
               (string-append "--with-etcdir=" %output ,icon-dir))
         #:phases
         (modify-phases %standard-phases
           (add-after 'configure 'patch-mew-icon-directory
             (lambda* (#:key outputs #:allow-other-keys)
               (emacs-substitute-sexps "mew-key.el"
                 ("(def.* mew-icon-directory"
                  `(progn
                    (add-to-list 'image-load-path 'mew-icon-directory)
                    ,(string-append (assoc-ref outputs "out") ,icon-dir))))
               #t))
           (add-after 'install 'generate-autoloads
             (lambda* (#:key outputs #:allow-other-keys)
               (emacs-generate-autoloads
                "mew" (string-append (assoc-ref outputs "out") ,elisp-dir))
               #t)))
         #:tests? #f)))
    (home-page "https://mew.org")
    (synopsis "Emacs e-mail client")
    (description "Mew (Messaging in the Emacs World) is a user interface
for text messages, multimedia messages (MIME), news articles and
security functionality including PGP, S/MIME, SSH, and SSL.")
    (license bsd-3)))

(define-public mu
  (package
    (name "mu")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/djcb/mu/releases/"
                                  "download/v" version "/mu-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "04x5azl19gszw2h7argq666gf9xs4hy9q7w9cbqxvy08n56xqsln"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")             ; for gtester
       ("emacs" ,emacs-minimal)
       ("tzdata" ,tzdata-for-tests)))   ;for mu/test/test-mu-query.c
    ;; TODO: Add webkit and gtk to build the mug GUI.
    (inputs
     `(("xapian" ,xapian)
       ("guile" ,guile-2.2)
       ("glib" ,glib)
       ("gmime" ,gmime-2.6)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-configure
           ;; By default, elisp code goes to "share/emacs/site-lisp/mu4e",
           ;; so our Emacs package can't find it.  Setting "--with-lispdir"
           ;; configure flag doesn't help because "mu4e" will be added to
           ;; the lispdir anyway, so we have to modify "configure.ac".
           (lambda _
             (substitute* "configure"
               (("^ +lispdir=\"\\$\\{lispdir\\}/mu4e/\".*") "")
               ;; Use latest Guile
               (("guile-2.0") "guile-2.2"))
             (substitute* '("guile/Makefile.in"
                            "guile/mu/Makefile.in")
               (("share/guile/site/2.0/") "share/guile/site/2.2/"))
             #t))
         (add-after 'patch-configure 'fix-date-tests
           ;; Loosen test tolerances to prevent failures caused by daylight
           ;; saving time (DST).  See: https://github.com/djcb/mu/issues/1214.
           (lambda _
             (substitute* "lib/parser/test-utils.cc"
               (("\\* 60 \\* 60, 1 },")
                "* 60 * 60, 3600 + 1 },"))
             #t))
         (add-before 'install 'fix-ffi
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "guile/mu.scm"
               (("\"libguile-mu\"")
                (format #f "\"~a/lib/libguile-mu\""
                        (assoc-ref outputs "out"))))
             #t))
         (add-before 'check 'check-tz-setup
           (lambda* (#:key inputs #:allow-other-keys)
             ;; For mu/test/test-mu-query.c
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t))
         (add-after 'install 'install-emacs-autoloads
           (lambda* (#:key outputs #:allow-other-keys)
             (emacs-generate-autoloads
              "mu4e"
              (string-append (assoc-ref outputs "out")
                             "/share/emacs/site-lisp"))
             #t)))))
    (home-page "http://www.djcbsoftware.nl/code/mu/")
    (synopsis "Quickly find emails")
    (description
     "Mu is a tool for dealing with e-mail messages stored in the
Maildir-format.  Mu's purpose in life is to help you to quickly find the
messages you need; in addition, it allows you to view messages, extract
attachments, create new maildirs, and so on.")
    (license gpl3+)))

(define-public alot
  (package
    (name "alot")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              ;; package author intends on distributing via github rather
              ;; than pypi:
              ;; https://github.com/pazz/alot/issues/877#issuecomment-230173331
              (uri (string-append "https://github.com/pazz/alot/archive/"
                                  version ".tar.gz"))
              (file-name (string-append "alot-" version ".tar.gz"))
              (sha256
               (base32
                "0wax30hjzmkqfml7hig1dqw1v1y63yc0cgbzl96x58b9h2ggqx3a"))))
    (build-system python-build-system)
    (arguments
     `(;; python 3 is currently unsupported, more info:
       ;; https://github.com/pazz/alot/blob/master/docs/source/faq.rst
       #:python ,python-2))
    (native-inputs
     `(("python2-mock" ,python2-mock)))
    (inputs
     `(("python2-magic" ,python2-magic)
       ("python2-configobj" ,python2-configobj)
       ("python2-twisted" ,python2-twisted)
       ("python2-urwid" ,python2-urwid)
       ("python2-urwidtrees" ,python2-urwidtrees)
       ("python2-pygpgme" ,python2-pygpgme)
       ("python2-notmuch" ,python2-notmuch)))
    (home-page "https://github.com/pazz/alot")
    (synopsis "Command-line MUA using @code{notmuch}")
    (description
     "Alot is an experimental terminal mail user agent (@dfn{MUA}) based on
@code{notmuch} mail.  It is written in Python using the @code{urwid} toolkit.")
    (license gpl3+)))

(define-public notifymuch
  (let
      ((commit "9d4aaf54599282ce80643b38195ff501120807f0")
       (revision "1"))
    (package
      (name "notifymuch")
      (version (string-append "0.1-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kspi/notifymuch.git")
               (commit commit)))
         (sha256
          (base32
           "1lssr7iv43mp5v6nzrfbqlfzx8jcc7m636wlfyhhnd8ydd39n6k4"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system python-build-system)
      (inputs
       `(("python-notmuch" ,python-notmuch)
         ("python-pygobject" ,python-pygobject)
         ("gobject-introspection" ,gobject-introspection)
         ("libnotify" ,libnotify)
         ("gtk+" ,gtk+)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-binary
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin/notifymuch")))
                 (wrap-program bin
                   `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))
                   `("GI_TYPELIB_PATH" ":" prefix
                     (,(getenv "GI_TYPELIB_PATH")
                      ,(string-append out "/lib/girepository-1.0")))))
               #t)))))
      (home-page "https://github.com/kspi/notifymuch")
      (synopsis "Displays notifications for changes in the notmuch email database")
      (description "notifymuch displays desktop notifications for messages in
the notmuch database.  The notifications are sent using libnotify to a
notification daemon.  The query to find messages to send a notification about
is configurable, and a notification for the same message will not be send
within a configurable period (defaults to 48 hours).  To use notifymuch, run
@command{notifymuch} after new mail is indexed, this can be automated by
invoking @command{notifymuch} from the post-new hook.")
      (license gpl3))))

(define-public notmuch
  (package
    (name "notmuch")
    (version "0.28.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://notmuchmail.org/releases/notmuch-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1v0ff6qqwj42p3n6qw30czzqi52nvgf3dn05vd7a03g39a5js8af"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build emacs-build-system) #:prefix emacs:)
                  (guix build utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-build-system)
                           (guix build emacs-utils))
       #:make-flags (list "V=1") ; Verbose test output.
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-notmuch-lib.el
                    (lambda _
                      (substitute* "emacs/notmuch-lib.el"
                        (("/bin/sh") (which "sh")))
                      #t))
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (setenv "CC" "gcc")
                      (setenv "CONFIG_SHELL" (which "sh"))

                      (let* ((out (assoc-ref outputs "out"))
                             (elisp
                              (string-append out "/share/emacs/site-lisp/guix.d/"
                                             ,name "-" ,version)))
                        (invoke "./configure"
                                (string-append "--prefix=" out)
                                (string-append "--emacslispdir=" elisp)
                                (string-append "--emacsetcdir=" elisp)))))
                  (add-before 'check 'prepare-test-environment
                    (lambda _
                      (setenv "TEST_CC" "gcc")
                      ;; Patch various inline shell invocations.
                      (substitute* (find-files "test" "\\.sh$")
                        (("/bin/sh") (which "sh")))
                      #t))
                  (add-after 'install 'make-autoloads
                    (assoc-ref emacs:%standard-phases 'make-autoloads)))))
    (native-inputs
     `(("bash-completion" ,bash-completion)
       ("emacs" ,emacs-no-x) ; Minimal lacks libxml, needed for some tests.
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("python-docutils" ,python2-docutils)
       ("python-sphinx" ,python2-sphinx)

       ;; The following are required for tests only.
       ("which" ,which)
       ("dtach" ,dtach)
       ("gnupg" ,gnupg)
       ("man" ,man-db)
       ("perl" ,perl)))
    (inputs
     `(("glib" ,glib)
       ("gmime" ,gmime)
       ("talloc" ,talloc)
       ("xapian" ,xapian)
       ("zlib" ,zlib)))
    (home-page "https://notmuchmail.org/")
    (synopsis "Thread-based email index, search, and tagging")
    (description
     "Notmuch is a command-line based program for indexing, searching, read-
ing, and tagging large collections of email messages.")
    (license gpl3+)))

(define-public notmuch-addrlookup-c
  ;; This commit includes a compatibility fix for notmuch-0.25, and is not
  ;; currently part of any release.  Please update this package when
  ;; notmuch-addrlookup-c-9 is released.
  (let ((commit "88f156d04990a71c6ad6fc2757b537b44e3c4d00")
        (revision "1"))          ;Guix package revision
    (package
      (name "notmuch-addrlookup-c")
      (version (string-append "8-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/aperezdc/notmuch-addrlookup-c.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0v0wzs7qzy4n1hbql8s10qrwgalcxdzbxf8pj6cii1pv2jwmkxbm"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f ; no tests
         #:make-flags (list "CC=gcc"
                            (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    ;; Remove vim code completion config, it's not needed to
                    ;; build (or be patched).
                    (add-before 'patch-source-shebangs 'delete-ycm-file
                                (lambda _ (delete-file ".ycm_extra_conf.py")))
                    (replace 'install
                             (lambda* (#:key outputs #:allow-other-keys)
                               (let ((bin (string-append
                                           (assoc-ref outputs "out") "/bin")))
                                 (install-file "notmuch-addrlookup" bin)))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("glib" ,glib)
         ("notmuch" ,notmuch)))
      (home-page "https://github.com/aperezdc/notmuch-addrlookup-c")
      (synopsis "Address lookup tool for Notmuch")
      (description "This is an address lookup tool using a Notmuch database,
useful for email address completion.")
      (license license:expat))))

(define-public python-notmuch
  (package
    (name "python-notmuch")
    (version (package-version notmuch))
    ;; Notmuch python bindings are now unavailable on pypi.  The
    ;; bindings are distributed via the notmuch release tarball.
    (source (package-source notmuch))
    (build-system python-build-system)
    (inputs `(("notmuch" ,notmuch)))
    (arguments
     `(#:tests? #f  ; no "test" target
       #:phases
       (modify-phases %standard-phases
         ;; This python package lives in a subdirectory of the notmuch source
         ;; tree, so chdir into it before building.
         (add-after 'unpack 'enter-python-dir
           (lambda _ (chdir "bindings/python") #t))
         ;; Make sure the correct notmuch shared library gets loaded.
         (add-before 'build 'set-libnotmuch-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((notmuch (assoc-ref inputs "notmuch")))
               (substitute* "notmuch/globals.py"
                 (("libnotmuch\\.so\\.")
                  (string-append notmuch "/lib/libnotmuch.so.")))
               #t))))))
    (home-page (package-home-page notmuch))
    (synopsis "Python bindings of the Notmuch mail indexing library")
    (description
     "This package provides Python bindings to use the Notmuch mail indexing
and search library.")
    (license gpl3+)))

(define-public python2-notmuch
  (package-with-python2 python-notmuch))

(define-public getmail
  (package
    (name "getmail")
    (version "5.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://pyropus.ca/software/getmail/old-versions/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "16nmvj80szr6yvcxxgmxn2lxqpjqqj4xg5a0b66zhvck6j42q3a6"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:python ,python-2))
    (home-page "http://pyropus.ca/software/getmail/")
    (synopsis "Mail retriever")
    (description
     "A flexible, extensible mail retrieval system with support for
POP3, IMAP4, SSL variants of both, maildirs, mboxrd files, external MDAs,
arbitrary message filtering, single-user and domain-mailboxes, and many other
useful features.")

    ;; License is specified in file '__init__.py'.
    (license gpl2)))

(define-public libetpan
  (package
    (name "libetpan")
    (version "1.9.2")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url  "https://github.com/dinhviethoa/libetpan.git")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
               (base32 "13jiy2ddxbp9f2mk1mip9sk8h97bva5m0pnq2mlvh5xhifs6gza4"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf-wrapper)
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; 'libetpan-config --libs' returns '-lssl -lcrypto -lsasl2', so these
     ;; libraries need to be propagated.
     `(("cyrus-sasl" ,cyrus-sasl)
       ("openssl" ,openssl)))
    (inputs
     `(("curl" ,curl)
       ("expat" ,expat)
       ("zlib" ,zlib)))
    (arguments
      '(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'autogen
            (lambda _
              (setenv "NOCONFIGURE" "true")
              (invoke "sh" "autogen.sh"))))
        #:configure-flags
        '("--disable-static" "--disable-db")))
    (home-page "https://www.etpan.org/libetpan.html")
    (synopsis "Portable middleware for email access")
    (description
     "The purpose of this mail library is to provide a portable, efficient
framework for different kinds of mail access: IMAP, SMTP, POP and NNTP.  It
provides an API for C language.  It's the low-level API used by MailCore and
MailCore 2.")
    (license bsd-3)))

(define-public compface
  (package
    (name "compface")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.heanet.ie/mirrors/"
                                  "ftp.xemacs.org/aux/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "09b89wg63hg502hsz592cd2h87wdprb1dq1k1y07n89hym2q56d6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis "Portrait image compressor")
    (description "This packages takes your 48x48x1 portrait image and
compresses it.")
    (home-page "http://www.cs.indiana.edu/pub/faces/")
    (license (x11-style "file://README"))))

(define-public claws-mail
  (package
    (name "claws-mail")
    (version "3.17.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.claws-mail.org/releases/claws-mail-" version
                    ".tar.xz"))
              (sha256
               (base32
                "1wnj6c9cbmhphs2l6wfvndkk2g08rmxw0sl2c8k1k008dxd1ykjh"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("bogofilter" ,bogofilter)
              ("curl" ,curl)
              ("dbus-glib" ,dbus-glib)
              ("enchant" ,enchant)
              ("expat" ,expat)
              ("ghostscript" ,ghostscript)
              ("hicolor-icon-theme" ,hicolor-icon-theme)
              ("gnupg" ,gnupg)
              ("gnutls" ,gnutls)
              ("gpgme" ,gpgme)
              ("gtk" ,gtk+-2)
              ("libarchive" ,libarchive)
              ("libcanberra" ,libcanberra)
              ("libetpan" ,libetpan)
              ("libical" ,libical)
              ("libnotify" ,libnotify)
              ("libsm" ,libsm)
              ("libxml2" ,libxml2)
              ("perl" ,perl)
              ("python-2" ,python-2)
              ("mime-info" ,shared-mime-info)))
    (arguments
      '(#:configure-flags
        '("--enable-gnutls" "--enable-pgpmime-plugin" "--enable-enchant")
        #:make-flags
        ;; Disable updating icon cache since it's done by the profile hook.
        ;; Conflict with other packages in the profile would be inevitable
        ;; otherwise.
        '("gtk_update_icon_cache=true")
        #:phases (modify-phases %standard-phases
                   (add-before 'build 'patch-mime
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "src/procmime.c"
                         (("/usr/share/mime/globs")
                          (string-append (assoc-ref inputs "mime-info")
                                         "/share/mime/globs"))))))))
    (synopsis "GTK-based Email client")
    (description
     "Claws-Mail is an email client (and news reader) based on GTK+.  The
appearance and interface are designed to be familiar to new users coming from
other popular email clients, as well as experienced users.  Almost all commands
are accessible with the keyboard.  Plus, Claws-Mail is extensible via addons
which can add many functionalities to the base client.")
    (home-page "https://www.claws-mail.org/")
    (license gpl3+))) ; most files are actually public domain or x11

(define-public msmtp
  (package
    (name "msmtp")
    (version "1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://marlam.de/msmtp/releases/"
                           "/msmtp-" version ".tar.xz"))
       (sha256
        (base32
         "1d4jdgrx4czp66nnwdsy938lzr4llhwyy0715pwg0j6h6gyyxciw"))))
    (build-system gnu-build-system)
    (inputs
     `(("libsecret" ,libsecret)
       ("gnutls" ,gnutls)
       ("zlib" ,zlib)
       ("gsasl" ,gsasl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://marlam.de/msmtp/")
    (arguments
     `(#:configure-flags (list "--with-libgsasl"
                               "--with-libidn"
                               "--with-tls=gnutls")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-msmtpq
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/msmtp"))
                    (msmtpq (string-append "scripts/msmtpq")))
               (install-file (string-append msmtpq "/msmtpq") bin)
               (install-file (string-append msmtpq "/msmtp-queue") bin)
               (install-file (string-append msmtpq "/README.msmtpq") doc)
               #t))))))
    (synopsis
     "Simple and easy to use SMTP client with decent sendmail compatibility")
    (description
     "msmtp is an SMTP client.  In the default mode, it transmits a mail to
an SMTP server (for example at a free mail provider) which takes care of further
delivery.")
    (license gpl3+)))

(define-public exim
  (package
    (name "exim")
    (version "4.90.1")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://ftp.exim.org/pub/exim/exim4/exim-"
                                 version ".tar.bz2")
                  (string-append "https://ftp.exim.org/pub/exim/exim4/old/exim-"
                                 version ".tar.bz2")))
       (sha256
        (base32
         "1w6blvvrd87c649j8xpag034md2w1ib0db9c4ijqbzc5dh2i1xfq"))))
    (build-system gnu-build-system)
    (inputs
     `(("bdb" ,bdb)
       ("gnutls" ,gnutls)
       ("gzip" ,gzip)
       ("bzip2" ,bzip2)
       ("xz" ,xz)
       ("perl" ,perl)
       ("libnsl" ,libnsl)
       ("libxt" ,libxt)
       ("libxaw" ,libxaw)))
    (native-inputs
     `(("pcre" ,pcre "bin")
       ("perl" ,perl)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; We'd use #:make-flags but the top-level Makefile calls others
           ;; recursively, so just set all variables this way.
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (substitute* '("Makefile" "OS/Makefile-Default")
               (("(RM_COMMAND=).*" all var)
                (string-append var "rm\n")))
             (copy-file "src/EDITME" "Local/Makefile")
             (copy-file "exim_monitor/EDITME" "Local/eximon.conf")
             (let ((out (assoc-ref outputs "out"))
                   (gzip (assoc-ref inputs "gzip"))
                   (bzip2 (assoc-ref inputs "bzip2"))
                   (xz (assoc-ref inputs "xz")))
               (substitute* '("Local/Makefile")
                 (("(BIN_DIRECTORY=).*" all var)
                  (string-append var out "/bin\n"))
                 (("(CONFIGURE_FILE=).*" all var)
                  (string-append var out "/etc/exim.conf\n"))
                 (("(EXIM_USER=).*" all var)
                  (string-append var "nobody\n"))
                 (("(FIXED_NEVER_USERS=).*" all var)
                  (string-append var "\n"))  ;XXX no root in build environment
                 (("(COMPRESS_COMMAND=).*" all var)
                  (string-append var gzip "/bin/gzip\n"))
                 (("(ZCAT_COMMAND=).*" all var)
                  (string-append var gzip "/bin/zcat\n")))
               ;; This file has hardcoded names for tools despite the zcat
               ;; configuration above.
               (substitute* '("src/exigrep.src")
                 (("'zcat'") (string-append "'" gzip "/bin/zcat'"))
                 (("'bzcat'") (string-append "'" bzip2 "/bin/bzcat'"))
                 (("'xzcat'") (string-append "'" xz "/bin/xzcat'"))
                 (("'lzma'") (string-append "'" xz "/bin/lzma'"))))
             #t))
         (add-before 'build 'fix-sh-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("scripts/lookups-Makefile" "scripts/reversion")
               (("SHELL=/bin/sh") "SHELL=sh"))
             (substitute* '("scripts/Configure-config.h")
               (("\\| /bin/sh") "| sh"))
             (let ((bash (assoc-ref inputs "bash")))
               (substitute* '("scripts/Configure-eximon")
                 (("#!/bin/sh") (string-append "#!" bash "/bin/sh"))))
             #t)))
       #:make-flags '("INSTALL_ARG=-no_chown")
       ;; No 'check' target.
       #:tests? #f))
    (home-page "https://www.exim.org/")
    (synopsis
     "Message Transfer Agent (MTA) developed at the University of Cambridge")
    (description
     "Exim is a message transfer agent (MTA) developed at the University of
Cambridge for use on Unix systems connected to the Internet.  In style it is
similar to Smail 3, but its facilities are more general.  There is a great
deal of flexibility in the way mail can be routed, and there are extensive
facilities for checking incoming mail.")
    (license gpl2+)))

(define-public dovecot
  (package
    (name "dovecot")
    (version "2.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.dovecot.org/releases/"
                           (version-major+minor version) "/"
                           "dovecot-" version ".tar.gz"))
       (sha256
        (base32 "1zxa9banams9nmk99sf1rqahr11cdqxhwi7hyz3ddxqidpn15qdz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("sqlite" ,sqlite)
       ("linux-pam" ,linux-pam)))
    (arguments
     `(#:configure-flags '("--sysconfdir=/etc"
                           "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-file-names
           (lambda _
             (substitute* "src/lib-program-client/test-program-client-local.c"
               (("(/bin/| )cat") (which "cat"))
               (("/bin/echo") (which "echo"))
               (("/bin/false") (which "false"))
               (("/bin/sh") (which "bash"))
               (("head") (which "head"))
               (("sleep") (which "sleep")))
             (substitute* (list "src/lib-smtp/test-bin/sendmail-exit-1.sh"
                                "src/lib-smtp/test-bin/sendmail-success.sh")
               (("cat") (which "cat")))
             #t))
         (replace 'install
           (lambda* (#:key make-flags #:allow-other-keys)
             ;; Simple hack to avoid installing a trivial README in /etc.
             (apply invoke "make" "install" "sysconfdir=/tmp/bogus"
                    make-flags))))))
    (home-page "https://www.dovecot.org")
    (synopsis "Secure POP3/IMAP server")
    (description
     "Dovecot is a mail server whose major goals are security and reliability.
It supports mbox/Maildir and its own dbox/mdbox formats.")
    ;; Most source files are covered by either lgpl2.1 or expat.  The SHA code
    ;; is covered by a variant of BSD-3, and UnicodeData.txt is covered by the
    ;; Unicode, Inc. License Agreement for Data Files and Software.
    (license (list lgpl2.1 license:expat (non-copyleft "file://COPYING")))))

(define-public dovecot-trees
  (package
    (name "dovecot-trees")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://0xacab.org/riseuplabs/trees/repository/"
                           "archive.tar.gz?ref=v" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rkk10b1bsjz979sc864vpgcdchy7yxwmyv4ik50lar1h6awdnrf"))
       (patches
        (search-patches "dovecot-trees-support-dovecot-2.3.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("libtool" ,libtool)
       ("dovecot" ,dovecot)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libsodium" ,libsodium)))
    (arguments
     `(#:tests? #f ;No tests exist.
       #:configure-flags (list (string-append "--with-dovecot="
                                              (assoc-ref %build-inputs "dovecot")
                                              "/lib/dovecot"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _
             (invoke "sh" "autogen.sh"))))))
    (home-page "https://0xacab.org/riseuplabs/trees")
    (synopsis "NaCL-based Dovecot email storage encryption plugin")
    (description
     "Technology for Resting Email Encrypted Storage (TREES) is a NaCL-based
Dovecot encryption plugin.  This plugin adds individually encrypted mail
storage to the Dovecot IMAP server.  It is inspired by Posteo's scrambler
which uses OpenSSL and RSA keypairs.  TREES works in a similar way, but uses
the Sodium crypto library (based on NaCL).

How it works:
@enumerate
@item On IMAP log in, the user's cleartext password is passed to the plugin.
@item The plugin creates an argon2 digest from the password.
@item This password digest is used as a symmetric secret to decrypt a libsodium secretbox.
@item Inside the secretbox is stored a Curve25519 private key.
@item The Curve25519 private key is used to decrypt each individual message,
using lidsodium sealed boxes.
@item New mail is encrypted as it arrives using the Curve25519 public key.
@end enumerate\n")
    (license agpl3)))

(define-public dovecot-libsodium-plugin
  (let ((commit "044de73c01c35385df0105f6b387bec5d5317ce7")
        (revision "1"))
    (package
      (name "dovecot-libsodium-plugin")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/LuckyFellow/dovecot-libsodium-plugin")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "13h07l7xy713zchnj2p9fhvq7fdl4zy1ai94li3ygkqjjj8hrgas"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("automake" ,automake)
         ("autoconf" ,autoconf)
         ("libtool" ,libtool)
         ("dovecot" ,dovecot)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("libsodium" ,libsodium)))
      (arguments
       `(#:tests? #f ;No tests exist.
         #:configure-flags (list (string-append "--with-dovecot="
                                                (assoc-ref %build-inputs "dovecot")
                                                "/lib/dovecot"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'autogen
             (lambda _
               (invoke "sh" "autogen.sh"))))))
      (home-page "https://github.com/LuckyFellow/dovecot-libsodium-plugin")
      (synopsis "Libsodium password hashing schemes plugin for Dovecot")
      (description
       "@code{dovecot-libsodium-plugin} provides libsodium password
hashing schemes plugin for @code{Dovecot}.")
      (license gpl3+))))

(define-public isync
  (package
    (name "isync")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/isync/isync/"
                           version "/isync-" version ".tar.gz"))
       (sha256 (base32
                "173wd7x8y5sp94slzwlnb7zhgs32r57zl9xspl2rf4g3fqwmhpwd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)))
    (inputs
     `(("bdb" ,bdb)
       ("cyrus-sasl" ,cyrus-sasl)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "http://isync.sourceforge.net/")
    (synopsis "Mailbox synchronization program")
    (description
     "isync/mbsync is a command-line tool for two-way synchronization of
mailboxes.  Currently Maildir and IMAP are supported types.")
    (license gpl2+)))

(define-public perl-email-abstract
  (package
    (name "perl-email-abstract")
    (version "3.008")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Abstract-" version ".tar.gz"))
       (sha256
        (base32
         "0h42rhvp769wb421cpbbg6v6xjp8iv86mvz70pqgfgf4nsn6jwgw"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-email-simple" ,perl-email-simple)
       ("perl-module-pluggable" ,perl-module-pluggable)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "https://metacpan.org/release/Email-Abstract")
    (synopsis "Interface to mail representations")
    (description "Email::Abstract provides module writers with the ability to
write simple, representation-independent mail handling code.")
    (license perl-license)))

(define-public perl-email-address
  (package
    (name "perl-email-address")
    (version "1.909")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Address-" version ".tar.gz"))
       (sha256
        (base32
         "0l7x6sl06j9ffgfz5f9vgms2b5axd4cgp5fj03ivb3kia4km6b3g"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Email-Address")
    (synopsis "Email address parsing and creation")
    (description "Email::Address implements a regex-based RFC 2822 parser that
locates email addresses in strings and returns a list of Email::Address
objects found.  Alternatively you may construct objects manually.")
    (license perl-license)))

(define-public perl-email-date-format
  (package
    (name "perl-email-date-format")
    (version "1.005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Date-Format-" version ".tar.gz"))
       (sha256
        (base32
         "012ivfwpnbl3wr50f9c6f4azhdlxnm31pdn72528g79v61z6372p"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Email-Date-Format")
    (synopsis "Produce RFC 2822 date strings")
    (description "Email::Date::Format provides a means for generating an RFC
2822 compliant datetime string.")
    (license perl-license)))

(define-public perl-email-messageid
  (package
    (name "perl-email-messageid")
    (version "1.406")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MessageID-" version ".tar.gz"))
       (sha256
        (base32
         "1f22sdnfq169qw1l0lg7y74pmiam7j9v95bggjnf3q4mygdmshpc"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Email-MessageID")
    (synopsis "Generate world unique message-ids")
    (description "Email::MessageID generates recommended message-ids to
identify a message uniquely.")
    (license perl-license)))

(define-public perl-email-mime
  (package
    (name "perl-email-mime")
    (version "1.946")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MIME-" version ".tar.gz"))
       (sha256
        (base32
         "0z1k3i0lzp2k421gc8f3wq0jbqflkbw2xqd2k7n7pmv56417kvk8"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-email-address" ,perl-email-address)
       ("perl-email-messageid" ,perl-email-messageid)
       ("perl-email-mime-contenttype" ,perl-email-mime-contenttype)
       ("perl-email-mime-encodings" ,perl-email-mime-encodings)
       ("perl-email-simple" ,perl-email-simple)
       ("perl-mime-types" ,perl-mime-types)
       ("perl-module-runtime" ,perl-module-runtime)))
    (home-page "https://metacpan.org/release/Email-MIME")
    (synopsis "MIME message handling")
    (description "Email::MIME is an extension of the Email::Simple module, to
handle MIME encoded messages.  It takes a message as a string, splits it up
into its constituent parts, and allows you access to various parts of the
message.  Headers are decoded from MIME encoding.")
    (license perl-license)))

(define-public perl-email-mime-contenttype
  (package
    (name "perl-email-mime-contenttype")
    (version "1.022")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MIME-ContentType-" version ".tar.gz"))
       (sha256
        (base32
         "042kxhs3bp1ab9z0mbr1wy21ld4lxd6v2a2mmrashqnsn2075fws"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page "https://metacpan.org/release/Email-MIME-ContentType")
    (synopsis "Parse MIME Content-Type headers")
    (description "Email::MIME::ContentType parses a MIME Content-Type
header.")
    (license perl-license)))

(define-public perl-email-mime-encodings
  (package
    (name "perl-email-mime-encodings")
    (version "1.315")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MIME-Encodings-" version ".tar.gz"))
       (sha256
        (base32
         "0p5b8g9gh35m8fqrpx60g4bp98rvwd02n5b0vm9wh7mk0xah8wac"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page "https://metacpan.org/release/Email-MIME-Encodings")
    (synopsis "Unified interface to MIME encoding and decoding")
    (description "This module wraps MIME::Base64 and MIME::QuotedPrint.")
    (license perl-license)))

(define-public perl-email-sender
  (package
    (name "perl-email-sender")
    (version "1.300031")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Sender-" version ".tar.gz"))
       (sha256
        (base32
         "052g0slw3h2lzn93j71fi47nfawww2aph4jhr3c860ji70lkf4n4"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)))
    (propagated-inputs
     `(("perl-email-abstract" ,perl-email-abstract)
       ("perl-email-address" ,perl-email-address)
       ("perl-email-simple" ,perl-email-simple)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-moo" ,perl-moo)
       ("perl-moox-types-mooselike" ,perl-moox-types-mooselike)
       ("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-throwable" ,perl-throwable)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "https://metacpan.org/release/Email-Sender")
    (synopsis "Perl library for sending email")
    (description "Email::Sender replaces the old and sometimes problematic
Email::Send library.")
    (license perl-license)))

(define-public perl-email-simple
  (package
    (name "perl-email-simple")
    (version "2.216")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "1m4brbjvalyp5kjqslqv4155dzwg977shxin208i7lc8236n6pyq"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-email-date-format" ,perl-email-date-format)))
    (home-page "https://metacpan.org/release/Email-Simple")
    (synopsis "Parsing of RFC 2822 messages")
    (description "Email::Simple provides simple parsing of RFC 2822 message
format and headers.")
    (license perl-license)))

(define-public libesmtp
  (package
    (name "libesmtp")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://pkgs.fedoraproject.org/repo/pkgs/"
                                 "libesmtp/libesmtp-" version ".tar.bz2/"
                                 "bf3915e627fd8f35524a8fdfeed979c8/libesmtp-"
                                 version ".tar.bz2")
                  ;; XXX This site is offline, so we fetch Fedora's cached copy
                  ;; of the source tarball.
                  (string-append "http://www.stafford.uklinux.net/libesmtp/libesmtp-"
                                 version ".tar.bz2")))
       (sha256
        (base32
         "02zbniyz7qys1jmx3ghx21kxmns1wc3hmv80gp7ag7yra9f1m9nh"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("openssl" ,openssl)))
    (home-page "http://www.stafford.uklinux.net/libesmtp/")
    (synopsis "Library for sending mail via remote hosts using SMTP")
    (description "libESMTP is an SMTP client which manages posting (or
submission of) electronic mail via a preconfigured Mail Transport Agent (MTA).
It may be used as part of a Mail User Agent (MUA) or other program that must
be able to post electronic mail where mail functionality may not be that
program's primary purpose.")
    (license (list lgpl2.1+ gpl2+))))

(define-public esmtp
  (package
    (name "esmtp")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andywingo/esmtp.git")
             (commit "01bf9fc")))
       (sha256
        (base32
         "1ay282rrl92h0m0m8z5zzjnwiiagi7c78aq2qvhia5mw7prwfyw2"))
       (file-name (string-append name "-" version "-checkout"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'bootstrap
                   (lambda _ (invoke "autoreconf" "-vfi"))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("libesmtp" ,libesmtp)))
    (home-page "https://sourceforge.net/projects/esmtp/")
    (synopsis "Relay-only mail transfer agent (MTA)")
    (description "Esmtp is a simple relay-only mail transfer agent built using
libESMTP.  It sends e-mail via a remote SMTP server using credentials from the
user's @file{$HOME/.esmtprc} configuration file; see the @command{esmtprc} man
page for more on configuration.  This package also provides minimal
compatibility shims for the @command{sendmail}, @command{mailq}, and
@command{newaliases} commands.")
    (license gpl2+)))

(define-public fdm
  (package
    (name "fdm")
    (version "1.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/nicm/fdm/releases/download/"
                                 version "/fdm-"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
               (base32 "054rscijahiza5f9qha79rg3siji3bk5mk10f8c2vqx7m4w6qh8n"))))
    (build-system gnu-build-system)
    (inputs
     `(("tdb" ,tdb)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://github.com/nicm/fdm")
    (synopsis "Mail Retrieval Agent (MRA) and Mail Delivery Agent (MDA)")
    (description "fdm is a program designed to fetch mail from POP3
or IMAP servers, or receive local mail from stdin, and
deliver it in various ways.")
    (license
     ;; Why point to a source file?  Well, all the individual files have a
     ;; copy of this license in their headers, but there's no seprate file
     ;; with that information.
     (non-copyleft "https://github.com/nicm/fdm/blob/master/command.c"))))


(define-public procmail
  (package
    (name "procmail")
    (version "3.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.fu-berlin.de/pub/unix/mail/procmail/procmail-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "05z1c803n5cppkcq99vkyd5myff904lf9sdgynfqngfk9nrpaz08"))
       ;; The following patch fixes an ambiguous definition of
       ;; getline() in formail.c.  The patch is provided by Debian as
       ;; patch 24.
       (patches (search-patches "procmail-ambiguous-getline-debian.patch"
                                "procmail-CVE-2014-3618.patch"
                                "procmail-CVE-2017-16844.patch"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda _
                      (substitute* "Makefile"
                        (("/bin/sh")
                         (which "sh"))
                        (("/usr")
                         (assoc-ref %outputs "out"))
                        (("/bin/rm")
                         (which "rm")))
                      #t)))
       #:tests? #f)) ;; There are no tests indicating a successful
    ;; build.  Some tests of basic locking mechanisms provided by the
    ;; file system are performed during 'make install'.  However, these
    ;; are performed before the actual build process.
    (build-system gnu-build-system)
    (inputs `(("exim" ,exim)))
    (home-page "http://www.procmail.org/")
    (synopsis "Versatile mail delivery agent (MDA)")
    (description "Procmail is a mail delivery agent (MDA) featuring support
for a variety of mailbox formats such as mbox, mh and maildir.  Incoming mail
can be sorted into separate files/directories and arbitrary commands can be
executed on mail arrival.  Procmail is considered stable, but is no longer
maintained.")
    (license gpl2+))) ;; procmail allows to choose the
                      ;; nonfree Artistic License 1.0
                      ;; as alternative to the GPL2+.
                      ;; This option is not listed here.

(define-public khard
  (package
    (name "khard")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri name version))
              (sha256
               (base32
                "1lyjiskc6ckjjylzr04dnm66p3cnn7vlysw9c27qls3y3ywx14zw"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/khard")))
               (copy-recursively "misc/khard" doc)
               #t))))))
    (propagated-inputs
     `(("python-atomicwrites" ,python-atomicwrites)
       ("python-configobj" ,python-configobj)
       ("python-pyyaml" ,python-pyyaml)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-unidecode" ,python-unidecode)
       ("python-vobject" ,python-vobject)))
    (synopsis "Console address book using CardDAV")
    (description "Khard is an address book for the console.  It creates, reads,
modifies and removes CardDAV address book entries at your local machine.  For
synchronizing with a remote address book, @command{vdirsyncer} is recommended.
Khard can also be used from within the email client @command{mutt}.")
    (home-page "https://github.com/scheibler/khard")
    (license gpl3+)))

(define-public perl-mail-spf
 (package
  (name "perl-mail-spf")
  (version "v2.9.0")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/J/JM/JMEHNLE/mail-spf/Mail-SPF-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0qk1rfgfm5drj4iyniiabrasrpqv570vzhgz66lwgb67y4amkjv1"))))
  (build-system perl-build-system)
  (native-inputs
    `(("perl-module-build" ,perl-module-build)
      ("perl-net-dns-resolver-programmable"
       ,perl-net-dns-resolver-programmable)))
  (arguments
   `(#:phases (modify-phases %standard-phases
       (add-before 'configure 'modify-Build.PL
         (lambda* (#:key outputs #:allow-other-keys)
           (substitute* "Build.PL"
             (("'/usr/sbin'") (string-append "'"
                                             (assoc-ref outputs "out")
                                             "/sbin'")))
             #t)))))
  (inputs
    `(("perl-error" ,perl-error)
      ("perl-net-dns" ,perl-net-dns)
      ("perl-netaddr-ip" ,perl-netaddr-ip)
      ("perl-uri" ,perl-uri)))
  (home-page
    "https://metacpan.org/release/Mail-SPF")
  (synopsis
    "Perl implementation of Sender Policy Framework")
  (description "Mail::SPF is the Sender Policy Framework implemented
in Perl.")
  (license bsd-3)))

(define-public mb2md
  (package
    (name "mb2md")
    (version "3.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://batleth.sapienti-sat.org/projects/mb2md/mb2md-"
                    version ".pl.gz"))
              (sha256
               (base32
                "0bvkky3c90738h3skd2f1b2yy5xzhl25cbh9w2dy97rs86ssjidg"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (out (assoc-ref %outputs "out"))
                (bin (string-append out "/bin"))
                (perl (assoc-ref %build-inputs "perl"))
                (gzip (assoc-ref %build-inputs "gzip"))
                (perl-timedate (assoc-ref %build-inputs "perl-timedate"))
                (perl5lib (string-append perl-timedate "/lib/perl5/site_perl")))
           (mkdir-p bin)
           (with-directory-excursion bin
             (copy-file source "mb2md.gz")
             (invoke (string-append gzip "/bin/gzip") "-d" "mb2md.gz")
             (substitute* "mb2md"
               (("#!/usr/bin/perl")
                (string-append "#!/usr/bin/perl -I " perl5lib)))
             (patch-shebang "mb2md" (list (string-append perl "/bin")))
             (chmod "mb2md" #o555))
           #t))))
    (native-inputs `(("gzip" ,gzip)))
    (inputs `(("perl" ,perl)
              ("perl-timedate" ,perl-timedate)))
    (home-page "http://batleth.sapienti-sat.org/projects/mb2md/")
    (synopsis "Mbox to maildir converter")
    (description
     "Mb2md is a Perl script that takes one or more mbox format files and
converts them to maildir format directories.")
    (license public-domain)))

(define-public mpop
  (package
    (name "mpop")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://marlam.de/mpop/releases/"
                           "mpop-" version ".tar.xz"))
       (sha256
        (base32
         "1rx5mhgqkm7swbynrhbsz32v85h0rydb4kqfgfs9jrznd9d14m2d"))))
    (build-system gnu-build-system)
    (inputs
     `(("gnutls" ,gnutls)
       ("libidn" ,libidn)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://marlam.de/mpop/")
    (synopsis "POP3 mail client")
    (description "mpop is a small and fast POP3 client suitable as a
fetchmail replacement.

mpop supports multiple accounts, header based mail filtering, delivery
to mbox files, maildir folders or a Mail Delivery Agent (MDA),
TLS/SSL, several authentication methods, Internationalized Domain
Names (IDN) and SOCKS proxies.")
    (license gpl3+)))

(define-public mhonarc
  (package
    (name "mhonarc")
    (version "2.6.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/EH/EHOOD/MHonArc-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0ll3v93yji334zqp6xfzfxc0127pmjcznmai1l5q6dzawrs2igzq"))))
    (build-system perl-build-system)
    (home-page "https://www.mhonarc.org/")
    (synopsis "Create HTML archives of mail/news messages")
    (description
     "MHonArc is a Perl mail-to-HTML converter.  MHonArc
provides HTML mail archiving with index, mail thread linking,
etc; plus other capabilities including support for MIME and
powerful user customization features.")
    (license gpl2+)))


(define-public sendmail
  (package
    (name "sendmail")
    (version "8.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.sendmail.org/pub/sendmail/sendmail."
             version ".tar.gz"))
       (sha256
        (base32
         "0fdl9ndmspqspdlmghzxlaqk56j3yajk52d7jxcg21b7sxglpy94"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'replace-/bin/sh
           (lambda _
             (substitute*
                 (append
                  (list "smrsh/smrsh.c" "sendmail/conf.c" "contrib/mailprio"
                        "contrib/mmuegel" "devtools/bin/configure.sh")
                  (find-files "." ".*\\.m4")
                  (find-files "." ".*\\.cf"))
               (("/bin/sh") (which "sh")))

             (substitute* "devtools/bin/Build"
               (("SHELL=/bin/sh") (string-append "SHELL=" (which "sh"))))
             #t))
         (replace 'configure
           (lambda _

             ;; Render harmless any attempts to chown or chgrp
             (substitute* "devtools/bin/install.sh"
               (("owner=\\$2") "owner=''")
               (("group=\\$2") "group=''"))

             (with-output-to-file "devtools/Site/site.config.m4"
               (lambda ()
                 (format #t "
define(`confCC', `gcc')
define(`confOPTIMIZE', `-g -O2')
define(`confLIBS', `-lresolv')
define(`confINSTALL', `~a/devtools/bin/install.sh')
define(`confDEPEND_TYPE', `CC-M')
define(`confINST_DEP', `')
" (getcwd))))
             #t))
         (replace 'build
           (lambda _
             (invoke "sh" "Build")
             (with-directory-excursion "cf/cf"
               (copy-file "generic-linux.mc" "sendmail.mc")
               (invoke "sh" "Build" "sendmail.cf"))
             #t))
         (add-before 'install 'pre-install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (mkdir-p (string-append out "/usr/bin"))
               (mkdir-p (string-append out "/usr/sbin"))
               (mkdir-p (string-append out "/etc/mail"))
               (setenv "DESTDIR" out)
               (with-directory-excursion "cf/cf"
                 (invoke "sh" "Build" "install-cf"))
               #t))))
       ;; There is no make check.  There are some post installation tests, but those
       ;; require root privileges
       #:tests? #f))
    (inputs
     `(("m4" ,m4)
       ("perl" ,perl)))
    (home-page "http://sendmail.org")
    (synopsis
     "Highly configurable Mail Transfer Agent (MTA)")
    (description
     "Sendmail is a mail transfer agent (MTA) originally developed by Eric
Allman.  It is highly configurable and supports many delivery methods and many
transfer protocols.")
    (license (non-copyleft "file://LICENSE"
                           "See LICENSE in the distribution."))))

(define-public opensmtpd
  (package
    (name "opensmtpd")
    (version "6.0.3p1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.opensmtpd.org/archives/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "10bsfsnlg9d9i6l2izdnxp05s3ri8fvwzqxvx1jmarc852382619"))
              ;; Fixed upstream: <github.com/OpenSMTPD/OpenSMTPD/pull/835>.
              (patches (search-patches "opensmtpd-fix-crash.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("bdb" ,bdb)
       ("libressl" ,libressl)
       ("libevent" ,libevent)
       ("libasr" ,libasr)
       ("linux-pam" ,linux-pam)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("groff" ,groff)))
    (arguments
     `(#:configure-flags
       (list "--with-table-db" "--with-auth-pam" "--localstatedir=/var"
             "--with-user-smtpd=smtpd" "--with-user-queue=smtpq"
             "--with-group-queue=smtpq"
             "--with-path-socket=/var/run" ; not default (./configure lies)
             "--with-path-CAfile=/etc/ssl/certs/ca-certificates.crt")
       #:phases
       (modify-phases %standard-phases
         ;; Fix some incorrectly hard-coded external tool file names.
         (add-after 'unpack 'patch-FHS-file-names
           (lambda _
             (substitute* "smtpd/smtpctl.c"
               (("/bin/cat") (which "cat"))
               (("/bin/sh") (which "sh")))
             #t))
         ;; OpenSMTPD provides a single utility smtpctl to control the daemon and
         ;; the local submission subsystem.  To accomodate systems that require
         ;; historical interfaces such as sendmail, newaliases or makemap, the
         ;; smtpctl utility can operate in compatibility mode if called with the
         ;; historical name.
         (add-after 'install 'install-compability-links
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin/")))
               (for-each (lambda (command)
                           (symlink "smtpctl" (string-append sbin command)))
                         '("makemap" "sendmail" "send-mail"
                           "newaliases" "mailq")))
             #t)))))
    (synopsis "Lightweight SMTP daemon")
    (description
     "OpenSMTPD is an implementation of the server-side SMTP protocol, with
some additional standard extensions.  It allows ordinary machines to exchange
e-mails with other systems speaking the SMTP protocol.")
    (home-page "https://www.opensmtpd.org")
    (license (list bsd-2 bsd-3 bsd-4 (non-copyleft "file://COPYING")
                   public-domain isc license:openssl))))

;; OpenSMTPd 6.4 introduced a new and incompatible configuration file format.
;; Use a different name, for now, to avoid auto-upgrades and broken mail boxes.
;; OPENSMTP-CONFIGURATION in (gnu services mail) will also need an overhaul.
(define-public opensmtpd-next
  (package
    (name "opensmtpd-next")
    (version "6.4.1p2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.opensmtpd.org/archives/"
                           "opensmtpd-" version ".tar.gz"))
       (sha256
        (base32 "0cppqlx4fk6l8rbim5symh2fm1kzshf421256g596j6c9f9q96xn"))))
    (build-system gnu-build-system)
    (inputs
     `(("bdb" ,bdb)
       ("libasr" ,libasr)
       ("libevent" ,libevent)
       ("libressl" ,libressl)
       ("linux-pam" ,linux-pam)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("groff" ,groff)))               ; for man pages
    (arguments
     `(#:configure-flags
       (list "--localstatedir=/var"
             ;; This is the default only if it exists at build time—it doesn't.
             "--with-path-socket=/var/run"
             "--with-path-CAfile=/etc/ssl/certs/ca-certificates.crt"
             "--with-user-smtpd=smtpd"
             "--with-user-queue=smtpq" "--with-group-queue=smtpq"
             "--with-auth-pam"
             "--with-table-db")
       #:phases
       (modify-phases %standard-phases
         ;; Fix some incorrectly hard-coded external tool file names.
         (add-after 'unpack 'patch-FHS-file-names
           (lambda _
             (substitute* "smtpd/smtpctl.c"
               ;; ‘gzcat’ is auto-detected at compile time, but ‘cat’ isn't.
               (("/bin/cat") (which "cat")))
             (substitute* "smtpd/mda_unpriv.c"
               (("/bin/sh") (which "sh")))
             #t))
         ;; OpenSMTPD provides a single smtpctl utility to control both the
         ;; daemon and the local submission subsystem.  To accomodate systems
         ;; that require historical interfaces such as sendmail, newaliases or
         ;; makemap, smtpctl operates in compatibility mode if called with the
         ;; historical name.
         (add-after 'install 'install-compability-links
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin/")))
               (for-each (lambda (command)
                           (symlink "smtpctl" (string-append sbin command)))
                         (list "mailq" "makemap" "newaliases"
                               "send-mail" "sendmail")))
             #t)))))
    (synopsis "Lightweight SMTP daemon")
    (description
     "OpenSMTPD is an implementation of server-side @acronym{SMTP, Simple Mail
Transfer Protocol}, with some additional standard extensions.  It allows
ordinary machines to exchange e-mails with other systems speaking the SMTP
protocol, or to deliver them to local users.

In order to simplify the use of SMTP, OpenSMTPD implements a smaller set of
functionality than those available in other SMTP daemons.  The objective is to
provide enough features to satisfy typical usage at the risk of unsuitability
to esoteric or niche requirements.")
    (home-page "https://www.opensmtpd.org")
    (license (list bsd-2 bsd-3 bsd-4 (non-copyleft "file://COPYING")
                   public-domain isc license:openssl))))

(define-public opensmtpd-extras
  (package
    (name "opensmtpd-extras")
    (version "5.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.opensmtpd.org/archives/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kld4hxgz792s0cb2gl7m2n618ikzqkj88w5dhaxdrxg4x2c4vdm"))))
    (build-system gnu-build-system)
    (inputs
     `(("libressl" ,libressl)
       ("libevent" ,libevent)
       ("libasr" ,libasr)
       ("python-2" ,python-2)
       ("opensmtpd" ,opensmtpd)
       ("perl" ,perl)
       ("lua" ,lua)
       ("postgresql" ,postgresql)
       ("sqlite" ,sqlite)
       ("linux-pam" ,linux-pam)))
    (native-inputs
     `(("bison" ,bison)
       ("pkg-config" ,pkg-config)
       ("groff" ,groff)
       ("automake" ,automake)
       ("autoconf" ,autoconf)))
    (arguments
     `(;; We have to configure it like this because the default checks for for example
       ;; python in /usr/local/bin, /usr/bin and fails otherwise.
       #:configure-flags (list
                          "--with-filter-clamav"    "--with-filter-dkim-signer"
                          "--with-filter-dnsbl"     "--with-filter-lua"
                          "--with-filter-monkey"    "--with-filter-pause"
                          "--with-filter-perl"      "--with-filter-python"
                          "--with-filter-regex"     "--with-filter-spamassassin"
                          "--with-filter-stub"      "--with-filter-trace"
                          "--with-filter-void"

                          "--with-queue-null"       "--with-queue-python"
                          "--with-queue-ram"        "--with-queue-stub"

                          "--with-scheduler-python" "--with-scheduler-ram"
                          "--with-scheduler-stub"

                          "--with-table-ldap"       ; "--with-table-mysql"
                          "--with-table-passwd"     "--with-table-postgres"
                          "--with-table-python"     "--with-table-socketmap"
                          "--with-table-sqlite"     "--with-table-stub"
                          ;;"--with-table-redis"    ; TODO: package hiredis

                          "--with-user=smtpd"       "--with-privsep-user=smtpd"
                          "--localstatedir=/var"    "--sysconfdir=/etc"
                          "--with-lua-type=lua"     ; can use lua or luajit

                          (string-append "--with-python="
                                         (assoc-ref %build-inputs "python-2"))
                          (string-append "--with-lua="
                                         (assoc-ref %build-inputs "lua")))))
    (license (list bsd-2 bsd-3 bsd-4
                   public-domain isc license:openssl))
    (synopsis "Extra tables, filters, and various other addons for OpenSMTPD")
    (description
     "This package provides extra tables, filters, and various other addons
for OpenSMTPD to extend its functionality.")
    (home-page "https://www.opensmtpd.org")))

(define-public python-mailmanclient
  (package
    (name "python-mailmanclient")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mailmanclient" version))
       (sha256
        (base32
         "0fdfs5g3pf30v2i7w18pdkv9xnfxmfcv66mzv56dck0a1igq07m3"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Requires mailman running
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-httplib2" ,python-httplib2)))
    (home-page "https://launchpad.net/mailman.client")
    (synopsis "Python bindings for the Mailman 3 REST API")
    (description
     "The mailmanclient library provides official Python bindings for
the GNU Mailman 3 REST API.")
    (license lgpl3+)))

(define-public python2-mailmanclient
  (package-with-python2 python-mailmanclient))

(define-public mlmmj
  (package
    (name "mlmmj")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://mlmmj.org/releases/mlmmj-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0hpj10qad821ci11si8xc2qnmkzfn90y13s43fm4fca38f0qjp8w"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl))) ; For "contrib/web/"
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       ;; mlmmj-receive-strip is a replacement for mlmmj-receive
       ;; It opens the files control/mimedeny and control/mimestrip to get a list
       ;; of mimetypes for parts of multipart/mime messages that should be denied
       ;; or stripped. The parts then get stripped directly when the mail is
       ;; received. mlmmj-receive-strip also appends an extra header
       ;; X-ThisMailContainsUnwantedMimeParts: Y when the mail contains unwanted
       ;; mime parts
       (list "--enable-receive-strip")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-contrib
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/mlmmj"))
                    (contrib (string-append share "/contrib/web"))
                    (texts (string-append share "/listtexts")))
               (copy-recursively "contrib/web/" contrib)
               (copy-recursively "listtexts" texts)
               (rename-file texts (string-append share "/texts"))
               #t))))))
    (home-page "http://mlmmj.org")
    (synopsis "Mailing list managing made joyful")
    (description
     "Mlmmj is a simple and slim mailing list manager (MLM) inspired by ezmlm.
It works with many different Mail Transport Agents (MTAs) and is simple for a
system administrator to install, configure and integrate with other software.
As it uses very few resources, and requires no daemons, it is ideal for
installation on systems where resources are limited.  Its features include:
@enumerate
@item Archive, Custom headers / footer,
@item Fully automated bounce handling (similar to ezmlm),
@item Complete requeueing functionality, Moderation functionality, Subject prefix,
@item Subscribers only posting, Regular expression access control,
@item Functionality to retrieve old posts, Web interface, Digests,
@item No-mail subscription, VERP support,
@item Delivery Status Notification (RFC1891) support,
@item Rich and customisable texts for automated operations.
@end enumerate\n")
    (license license:expat)))

(define-public python2-django-mailman3
  (package
    (name "python2-django-mailman3")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-mailman3" version))
       (sha256
        (base32
         "1xjdkgfjwhgyrp5nxw65dcpcsr98ygj6856sp0bwkrmyxpd1xxk2"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "django-admin"
                     "test"
                     "--settings=django_mailman3.tests.settings_test"
                     "django_mailman3"))))
       #:python ,python-2))
    (inputs
     `(("python2-django" ,python2-django)))
    (propagated-inputs
     `(("python2-requests" ,python2-requests)
       ("python2-requests-oauthlib" ,python2-requests-oauthlib)
       ("python2-openid" ,python2-openid)
       ("python2-mailmanclient" ,python2-mailmanclient)
       ("python2-django-allauth" ,python2-django-allauth)
       ("python2-django-gravatar2" ,python2-django-gravatar2)
       ("python2-pytz" ,python2-pytz)))
    (home-page "https://gitlab.com/mailman/django-mailman3")
    (synopsis "Django library for Mailman UIs")
    (description
     "Libraries and templates for Django-based interfaces
interacting with Mailman.")
    (license gpl3+)))

(define-public postorius
  (package
    (name "postorius")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "postorius" version "+post2.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wymcpv2icjjy8h1ni52p6dr7wwxf71ivqgbqhzx4i82yqphcaq5"))))
    (build-system python-build-system)
    (arguments
     `(; One test dependency relies on Persona, which was shut down in
       ;; November 2016.
       #:tests? #f
       ;; The part of the frontend of Mailman is still python 2.7.
       #:python ,python-2))
    (inputs
     `(("python2-django" ,python2-django)
       ("python2-django-mailman3" ,python2-django-mailman3)
       ("python2-mailmanclient" ,python2-mailmanclient)))
    (home-page "https://gitlab.com/mailman/postorius")
    (synopsis "Web user interface for GNU Mailman")
    (description
     "Postorius is a Django app which provides a web user interface
to access GNU Mailman.")
    (license (list gpl3+ lgpl3+))))

(define-public blists
  (package
    (name "blists")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.openwall.net/pub/projects/"
                           "blists/blists-" version ".tar.gz"))
       (sha256
        (base32
         "1xll5wn7py3bbncbwrj172f56nz75c9gwfsa80rwd96ss9gfmp3c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "bindex" bin)
               (install-file "bit" bin)
               #t))))))
    (home-page "http://www.openwall.com/blists/")
    (synopsis "Web interface to mailing list archives")
    (description
     "Blists is a web interface to mailing list archives that works off
indexed mbox files.  There are two programs: @code{bindex} and @code{bit}.
@code{bindex} generates or updates the index file (incremental updates
are supported).  @code{bit} is a CGI/SSI program that generates web pages
on the fly.  Both programs are written in C and are very fast.")
    (license license:expat)))

(define-public swaks
  (package
    (name "swaks")
    (version "20181104.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://jetmore.org/john/code/swaks/files/swaks-"
             version ".tar.gz"))
       (sha256
        (base32
         "0n1yd27xcyb1ylp5gln3yv5gzi9r377hjy1j32367kgb3247ygq2"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-net-dns" ,perl-net-dns)
       ("perl-net-ssleay" ,perl-net-ssleay)))
    (arguments
     `(#:tests? #f ; No tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "pod2man" "doc/ref.pod" "swaks.1")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "swaks" (string-append out "/bin"))
               (install-file "swaks.1" (string-append out "/share/man/man1")))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/swaks")
               `("PERL5LIB" ":" = (,(getenv "PERL5LIB"))))
             #t)))))
    (home-page "https://jetmore.org/john/code/swaks/")
    (synopsis "Featureful SMTP test tool")
    (description "Swaks is a flexible, scriptable, transaction-oriented SMTP
test tool.  It handles SMTP features and extensions such as TLS,
authentication, and pipelining; multiple versions of the SMTP protocol
including SMTP, ESMTP, and LMTP; and multiple transport methods including
unix-domain sockets, internet-domain sockets, and pipes to spawned processes.
Options can be specified in environment variables, configuration files, and
the command line allowing maximum configurability and ease of use for
operators and scripters.")
    (license gpl2+)))

(define-public alpine
  (package
    (name "alpine")
    (version "2.21.9999")
    (source
     (origin
       (method url-fetch)
       ;; There are two versions: the plain continuation of Alpine without extra
       ;; patches and the version which adds extra fixes. Every distro uses
       ;; the patched version, and so do we to not break expectations.
       ;; http://alpine.freeiz.com/alpine/readme/README.patches
       (uri (string-append "http://repo.or.cz/alpine.git/snapshot/"
                           "d3e6f3932f2af9deca8eed09e30a55e9bd524362.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0w4qyybfdxi29r2z3giq0by6aa6v6nrgibh1xgv4d1vwwq4hw35w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc")
       #:configure-flags (list (string-append "--with-ssl-include-dir="
                                              (assoc-ref %build-inputs "openssl")
                                              "/include/openssl")
                               (string-append "--with-ssl-dir="
                                              (assoc-ref %build-inputs "openssl"))
                               (string-append "--with-ssl-certs-dir="
                                              "/etc/ssl/certs/")
                               (string-append "--with-ssl-lib-dir="
                                              (assoc-ref %build-inputs "openssl")
                                              "/lib")
                               (string-append "--with-interactive-spellcheck="
                                              (assoc-ref %build-inputs "aspell")
                                              "/bin/aspell"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; This removes time-dependent code to make alpine reproducible.
             (substitute* "pico/blddate.c"
               (("%02d-%s-%d") "1970-01-01"))
             (substitute* (list "alpine/Makefile.in"
                                "web/src/alpined.d/Makefile.in")
               (("`date`") "1970-01-01"))
             #t)))))
    (inputs
     `(("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("gnutls" ,gnutls)
       ("openldap" ,openldap)
       ("cyrus-sasl" ,cyrus-sasl)
       ("mit-krb5" ,mit-krb5)
       ("aspell" ,aspell)
       ("tcl" ,tcl)
       ("linux-pam" ,linux-pam)))
    (home-page "http://repo.or.cz/alpine.git")
    (synopsis "Alternatively Licensed Program for Internet News and Email")
    (description
     "Alpine is a text-based mail and news client.  Alpine includes several
tools and applications:
@enumerate
@item alpine, the Alpine mailer
@item pico, the standalone text editor, GNU nano's predecessor
@item pilot, the standalone file system navigator
@end enumerate\n")
    (license asl2.0)))

(define-public balsa
  (package
    (name "balsa")
    (version "2.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pawsa.fedorapeople.org/balsa/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32
         "17k6wcsl8gki7cskr3hhmfj6n54rha8ca3b6fzd8blsl5shsankx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '(;; Balsa tries to install additional MIME icons
         ;; under gtk+ directory.
         "--enable-extra-mimeicons=no"
         "--with-gtksourceview"
         "--with-canberra"
         "--with-spell-checker=gtkspell"
         "--with-gpgme"
         "--with-sqlite"
         "--with-compface"
         "--with-ldap")))
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("enchant" ,enchant)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gmime" ,gmime-2.6)
       ("gnutls" ,gnutls)
       ("gpgme" ,gpgme)
       ("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("gtkspell3" ,gtkspell3)
       ("libcanberra" ,libcanberra)
       ("libesmtp" ,libesmtp)
       ("libnotify" ,libnotify)
       ("openldap" ,openldap)
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk)))
    (native-inputs
     `(("compface" ,compface)
       ("glib" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("yelp-tools" ,yelp-tools)))
    (home-page "https://pawsa.fedorapeople.org/balsa")
    (synopsis "E-mail client for GNOME")
    (description "Balsa is a highly configurable and robust mail client for
the GNOME desktop.  It supports both POP3 and IMAP servers as well as the
mbox, maildir and mh local mailbox formats.  Balsa also supports SMTP and/or
the use of a local MTA such as Sendmail.")
    (license gpl3+)))

(define-public afew
  (package
    (name "afew")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "afew" version))
       (sha256
        (base32
         "121w7bd53xyibllxxbfykjj76n81kn1vgjqd22izyh67y8qyyk5r"))))
    (build-system python-build-system)
    (inputs
     `(("python-chardet" ,python-chardet)
       ("python-notmuch" ,python-notmuch)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/afewmail/afew")
    (synopsis "Initial tagging script for notmuch mail")
    (description "afew is an initial tagging script for notmuch mail.  It
provides automatic tagging each time new mail is registered with notmuch.  It
can add tags based on email headers or Maildir folders and can handle spam and
killed threads.")
    (license isc)))

(define-public pan
  (package
    (name "pan")
    (version "0.145")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://pan.rebelbase.com/download/releases/"
                           version "/source/" name "-" version ".tar.bz2"))
       (sha256
        (base32
         "1b4wamv33hprghcjk903bpvnd233yxyrm18qnh13alc8h1553nk8"))))
    (arguments
     `(#:configure-flags '("--with-gtk3" "--with-gtkspell" "--with-gnutls"
                           "--enable-libnotify" "--enable-manual"
                           "--enable-gkr")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-gpg2
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "pan/usenet-utils/gpg.cc"
               (("\"gpg2\"") (string-append "\""
                                            (assoc-ref inputs "gnupg")
                                            "/bin/gpg\"")))
             #t)))))
    (inputs
     `(("gmime" ,gmime-2.6)
       ("gnupg" ,gnupg)
       ("gnutls" ,gnutls)
       ("gtk+" ,gtk+)
       ("gtkspell3" ,gtkspell3)
       ("libnotify" ,libnotify)
       ("libsecret" ,libsecret)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gettext-minimal" ,gettext-minimal)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (build-system gnu-build-system)
    (home-page "http://pan.rebelbase.com/")
    (synopsis "Pan newsreader")
    (description "@code{pan} is a Usenet newsreader that's good at both text
and binaries. It supports offline reading, scoring and killfiles, yEnc, NZB,
PGP handling, multiple servers, and secure connections.")
    ;; License of the docs: fdl-1.1; Others: gpl2.
    (license (list fdl1.1+ gpl2))))

(define-public imapfilter
  (package
    (name "imapfilter")
    (version "2.6.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lefcha/imapfilter.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vzpc54fjf5vb5vx5w0fl20xvx1k9cg6a3hbl86mm8kwsqf3wrab"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-include-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pcre (assoc-ref inputs "pcre")))
               (substitute* "src/Makefile"
                 (("INCDIRS =")
                  (string-append "INCDIRS ="
                                 "-I" pcre "/include")))
               #t))))))
    (native-inputs
     `(("lua" ,lua)
       ("pcre" ,pcre)
       ("openssl" ,openssl)))
    (home-page "https://github.com/lefcha/imapfilter")
    (synopsis "IMAP mail filtering utility")
    (description "IMAPFilter is a mail filtering utility.  It connects
to remote mail servers using IMAP, sends searching queries to the server and
processes mailboxes based on the results.  It can be used to delete, copy,
move, flag, etc. messages residing in mailboxes at the same or different mail
servers.  The 4rev1 and 4 versions of IMAP are supported.")
    (license license:expat)))

(define-public urlscan
  (package
    (name "urlscan")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "urlscan" version))
        (sha256
         (base32
          "1zldck7vnp7z04aacbx3cprf5kzha4gfhjmss4iv2lh5nccxjfzx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-urwid" ,python-urwid)))
    (home-page "https://github.com/firecat53/urlscan")
    (synopsis "View/select the URLs in an email message or file")
    (description
     "Urlscan is a small program that is designed to integrate with the
@code{mutt} mailreader to allow you to easily launch a Web browser for URLs
contained in email messages.  It parses an email message or file and scans it
for URLs and email addresses.  It then displays the URLs and their context
within the message, and allows you to choose one or more URLs to send to your
Web browser.  Alternatively, it send a list of all URLs to stdout.  It is a
replacement for the @code{urlview} program.")
    (license gpl2)))

(define-public tnef
  (package
    (name "tnef")
    (version "1.4.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/verdammelt/tnef.git")
             (commit version)))
       (sha256
        (base32
         "0cq2xh5wd74qn6k2nnw5rayxgqhjl3jbzf4zlc4babcwxrv32ldh"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (arguments `(#:parallel-tests? #f)) ;tests are side-effect'y
    (home-page "https://github.com/verdammelt/tnef")
    (synopsis "Unpack @code{application/ms-tnef} attachments")
    (description
     "TNEF is a tar-like program that unpacks MIME attachments of type
@code{application/ms-tnef}.")
    (license gpl2+)))

(define-public mumi
  (let ((commit "ea5a738010148284aed211da953ad670003aefea")
        (revision "3"))
    (package
      (name "mumi")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.elephly.net/software/mumi.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ci5x8dqjmp74w33q2dbs5csxp4ilfmc1xxaa8q2jnh52d7597hl"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-executable
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (scm (string-append out "/share/guile/site/2.2"))
                      (go  (string-append out "/lib/guile/2.2/site-ccache")))
                 (wrap-program (string-append bin "/mumi")
                   `("GUILE_LOAD_PATH" ":" prefix
                     (,scm ,(getenv "GUILE_LOAD_PATH")))
                   `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                     (,go ,(getenv "GUILE_LOAD_COMPILED_PATH"))))
                 #t))))))
      (inputs
       `(("guile-debbugs" ,guile-debbugs-next)
         ("guile-email" ,guile-email)
         ("guile-fibers" ,guile-fibers)
         ("guile-json" ,guile-json)
         ("guile-syntax-highlight" ,guile-syntax-highlight)
         ("guile" ,guile-2.2)))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)))
      (home-page "https://git.elephly.net/software/mumi.git")
      (synopsis "Debbugs web interface")
      (description "Mumi is a Debbugs web interface.")
      (license agpl3+))))

(define-public ytnef
  (package
    (name "ytnef")
    (version "1.9.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Yeraze/ytnef.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07h48s5qf08503pp9kafqbwipdqghiif22ghki7z8j67gyp04l6l"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/Yeraze/ytnef/")
    (synopsis "TNEF stream reader for winmail.dat files")
    (description "This package provides a TNEF stream reader library and
related tools to process winmail.dat files.")
    (license gpl2+)))
