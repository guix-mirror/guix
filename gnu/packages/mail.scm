;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2014 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2014 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages search)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses)
                #:select (gpl2 gpl2+ gpl3+ lgpl2.1 lgpl2.1+ lgpl3+ non-copyleft
                          (expat . license:expat)))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python))

(define-public mailutils
  (package
    (name "mailutils")
    (version "2.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/mailutils/mailutils-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0szbqa12zqzldqyw97lxqax3ja2adis83i7brdfsxmrfw68iaf65"))
             (patches (list (search-patch "m4-gets-undeclared.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(;; TODO: Add `--with-sql'.
       #:phases (alist-cons-before
                 'build 'pre-build
                 (lambda _
                   ;; Use Guile 2.0's public API.
                   (substitute* "libmu_scm/mu_message.c"
                     (("scm_i_string_length")
                      "scm_c_string_length"))

                   ;; This file should be generated to use the right
                   ;; value of $(libdir) et al.
                   (delete-file "libmu_scm/mailutils.scm")

                   ;; Use the right file name for `cat'.
                   (substitute* "testsuite/lib/mailutils.exp"
                     (("/bin/cat")
                      (which "cat"))))
                 %standard-phases)
       #:parallel-tests? #f))
    (inputs
     `(("dejagnu" ,dejagnu)
       ("m4" ,m4)
       ("texinfo" ,texinfo)
       ("guile" ,guile-2.0)
       ("gnutls" ,gnutls)
       ("ncurses" ,ncurses)

       ;; With Readline 6.3, examples/pop3client.c fails to build because it
       ;; uses the now undefined 'CPPFunction' type.
       ("readline" ,readline-6.2)

       ("linux-pam" ,linux-pam)
       ("libltdl" ,libltdl)
       ("gdbm" ,gdbm)))
    (home-page "http://www.gnu.org/software/mailutils/")
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
    (version "1.5.23")
    (source (origin
             (method url-fetch)
             (uri (list ;; Temporarily put bitbucket first, because
                        ;; ftp.mutt.org has been down for a while.
                        (string-append "https://bitbucket.org/mutt/mutt/downloads/mutt-"
                                       version ".tar.gz")
                        (string-append "ftp://ftp.mutt.org/mutt/devel/mutt-"
                                       version ".tar.gz")))
             (sha256
              (base32
               "0dzx4qk50pjfsb6cs5jahng96a52k12f7pm0sc78iqdrawg71w1s"))
             (patches (map search-patch
                           '("mutt-CVE-2014-9116.patch"
                             "mutt-store-references.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("gpgme" ,gpgme)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("perl" ,perl)))
    (arguments
     `(#:configure-flags '("--enable-smtp"
                           "--enable-imap"
                           "--enable-pop"
                           "--enable-gpgme"
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

(define-public gmime
  (package
    (name "gmime")
    (version "2.6.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gmime/"
                                  (version-major+minor version)
                                  "/gmime-" version ".tar.xz"))
              (sha256
               (base32
                "0jm1fgbjgh496rsc0il2y46qd4bqq2ln9168p4zzh68mk4ml1yxg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gnupg" ,gnupg)))               ; for tests only
    (inputs `(("glib" ,glib)
              ("gpgme" ,gpgme)
              ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (alist-cons-after
        'unpack 'patch-paths-in-tests
        (lambda _
          ;; The test programs run several programs using 'system' with
          ;; hard-coded paths.  Here we patch them all.  We also change "gpg"
          ;; to "gpg2".  We use ISO-8859-1 here because test-iconv.c contains
          ;; raw byte sequences in several different encodings.
          (with-fluids ((%default-port-encoding #f))
            (substitute* (find-files "tests" "\\.c$")
              (("(system *\\(\")(/[^ ]*)" all pre prog-path)
               (let* ((base (basename prog-path))
                      (prog (which (if (string=? base "gpg") "gpg2" base))))
                 (string-append pre (or prog (error "not found: " base))))))))
        %standard-phases)))
    (home-page "http://spruce.sourceforge.net/gmime/")
    (synopsis "MIME message parser and creator library")
    (description
     "GMime provides a core library and set of utilities which may be used for
the creation and parsing of messages using the Multipurpose Internet Mail
Extension (MIME).")
    (license (list lgpl2.1+ gpl2+ gpl3+))))

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
     '(#:phases (alist-cons-before
                 'check 'pre-check
                 (lambda _
                   (substitute* "src/tests/t.frame"
                     (("GREP=/bin/grep")
                      (string-append "GREP=" (which "grep") "\n"))))
                 %standard-phases)))
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
    (version "6.5.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/OfflineIMAP/offlineimap/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "00k84qagph3xnxss6rkxm61x07ngz8fvffx4z9jyw5baf3cdd32p"))))
    (build-system python-build-system)
    (native-inputs `(("python" ,python-2)))
    (arguments
     ;; The setup.py script expects python-2.
     `(#:python ,python-2
      ;; Tests require a modifiable IMAP account.
       #:tests? #f))
    (home-page "http://www.offlineimap.org")
    (synopsis "Synch emails between two repositories")
    (description
     "OfflineImap synchronizes emails between two repositories, so that you
can read the same mailbox from multiple computers.  It supports IMAP as REMOTE
repository and Maildir/IMAP as LOCAL repository.")
    (license gpl2+)))

(define-public mu
  (package
    (name "mu")
    (version "0.9.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/djcb/mu/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append "mu-" version ".tar.gz"))
              (sha256
               (base32
                "1bxryacmas2llj68m2dv8dr1vwx8f5k2i2azh69jajkpqx7i4wdq"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")             ; for gtester
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("texinfo" ,texinfo)))
    ;; TODO: Add webkit and gtk to build the mug GUI.
    (inputs
     `(("xapian" ,xapian)
       ("emacs" ,emacs-no-x)
       ("guile" ,guile-2.0)
       ("glib" ,glib)
       ("gmime" ,gmime)
       ("tzdata" ,tzdata)))             ;for mu/test/test-mu-query.c
    (arguments
     '(#:phases (alist-cons-after
                 'unpack 'autoreconf
                 (lambda _
                   (zero? (system* "autoreconf" "-vi")))
                 (alist-cons-before
                   'check 'check-tz-setup
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; For mu/test/test-mu-query.c
                     (setenv "TZDIR"
                             (string-append (assoc-ref inputs "tzdata")
                                            "/share/zoneinfo")))
                   %standard-phases))))
    (home-page "http://www.djcbsoftware.nl/code/mu/")
    (synopsis "Quickly find emails")
    (description
     "Mu is a tool for dealing with e-mail messages stored in the
Maildir-format.  Mu's purpose in life is to help you to quickly find the
messages you need; in addition, it allows you to view messages, extract
attachments, create new maildirs, and so on.")
    (license gpl3+)))

(define-public notmuch
  (package
    (name "notmuch")
    (version "0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://notmuchmail.org/releases/notmuch-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1szf6c44g209pcjq5nvfhlp3nzcm3lrcwv4spsxmwy13hiaccvrr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;; FIXME: Test suite hangs and times out.
       #:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (setenv "CC" "gcc")
                   (setenv "CONFIG_SHELL" (which "sh"))

                   ;; XXX Should python-docutils make a symlink
                   ;; for "rst2man" and other similar programs?
                   (substitute* '("configure" "doc/prerst2man.py")
                     ((" rst2man ") " rst2man.py "))

                   (let ((out (assoc-ref outputs "out")))
                     (zero? (system* "./configure"
                                     (string-append "--prefix=" out)))))
                 %standard-phases)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("python2-docutils" ,python2-docutils)
       ("bash-completion" ,bash-completion)))
    (inputs
     `(("emacs" ,emacs)
       ("glib" ,glib)
       ("gmime" ,gmime)
       ("talloc" ,talloc)
       ("xapian" ,xapian)
       ("zlib" ,zlib)))
    (home-page "http://notmuchmail.org/")
    (synopsis "Thread-based email index, search, and tagging")
    (description
     "Notmuch is a command-line based program for indexing, searching, read-
ing, and tagging large collections of email messages.")
    (license gpl3+)))

(define-public getmail
  (package
    (name "getmail")
    (version "4.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://pyropus.ca/software/getmail/old-versions/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "15rqmm25pq6ll8aaqh8h6pfdkpqs7y6yismb3h3w1bz8j292c8zl"))))
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
    (version "1.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/dinhviethoa/" name
                   "/archive/" version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
               (base32 "05qyqx2c1ppb1jnrs3m52i60f9xlxfxdmb9dnwg4vqjv8kwv2qkr"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,(autoconf-wrapper))
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
       ("expat" ,expat)))
    (arguments
      '(#:phases (alist-cons-after
                  'unpack 'autogen
                  (lambda _
                    (setenv "NOCONFIGURE" "true")
                    (zero? (system* "sh" "autogen.sh")))
                  %standard-phases)
        #:configure-flags
        '("--disable-static" "--disable-db")))
    (home-page "http://www.etpan.org/libetpan.html")
    (synopsis "Portable middleware for email access")
    (description
     "The purpose of this mail library is to provide a portable, efficient
framework for different kinds of mail access: IMAP, SMTP, POP and NNTP.  It
provides an API for C language.  It's the low-level API used by MailCore and
MailCore 2.")
    (license (non-copyleft "file://COPYING"))))

(define-public claws-mail
  (package
    (name "claws-mail")
    (version "3.11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/claws-mail/claws-mail-" version
                    ".tar.xz"))
              (sha256
               (base32 "0cyixz1jgfpi8abh9fbb8ylx9mcvw4jqj81cms666wpqr6v828yp"))))
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
              ("libnotify" ,libnotify)
              ("libsm" ,libsm)
              ("libxml2" ,libxml2)
              ("perl" ,perl)
              ("python-2" ,python-2)))
    (arguments
      '(#:configure-flags
        '("--enable-gnutls" "--enable-pgpmime-plugin" "--enable-enchant")))
    (synopsis "GTK-based Email client")
    (description
     "Claws-Mail is an email client (and news reader) based on GTK+.  The
appearance and interface are designed to be familiar to new users coming from
other popular email clients, as well as experienced users.  Almost all commands
are accessible with the keyboard.  Plus, Claws-Mail is extensible via addons
which can add many functionalities to the base client.")
    (home-page "http://www.claws-mail.org/")
    (license gpl3+))) ; most files are actually public domain or x11

(define-public msmtp
  (package
    (name "msmtp")
    (version "1.4.32")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/msmtp/msmtp-" version ".tar.bz2"))
       (sha256 (base32
                "122z38pv4q03w3mbnhrhg4w85a51258sfdg2ips0b6cgwz3wbw1b"))))
    (build-system gnu-build-system)
    (inputs
     `(("libidn" ,libidn)
       ("gnutls" ,gnutls)
       ("zlib" ,zlib)
       ("gsasl" ,gsasl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://msmtp.sourceforge.net/")
    (arguments
     `(#:configure-flags (list "--with-libgsasl"
                               "--with-libidn"
                               "--with-ssl=gnutls")))
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
    (version "4.85")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.exim.org/pub/exim/exim4/exim-" version ".tar.bz2"))
       (sha256
        (base32 "195a3ll5ck9viazf9pvgcyc0sziln5g0ggmlm6ax002lphmiy88k"))))
    (build-system gnu-build-system)
    (inputs
     `(("bdb" ,bdb)
       ("gnutls" ,gnutls)
       ("gzip" ,gzip)
       ("bzip2" ,bzip2)
       ("xz" ,xz)
       ("pcre" ,pcre)
       ("perl" ,perl)
       ("libxt" ,libxt)
       ("libxaw" ,libxaw)))
    (native-inputs
     `(("perl" ,perl)))
    (arguments
     '(#:phases
       (alist-replace
        'configure
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
              (("'lzma'") (string-append "'" xz "/bin/lzma'")))))
        (alist-cons-before
         'build 'fix-sh-paths
         (lambda* (#:key inputs #:allow-other-keys)
           (substitute* '("scripts/lookups-Makefile" "scripts/reversion")
             (("SHELL=/bin/sh") "SHELL=sh"))
           (substitute* '("scripts/Configure-config.h")
             (("\\| /bin/sh") "| sh"))
           (let ((bash (assoc-ref inputs "bash")))
             (substitute* '("scripts/Configure-eximon")
               (("#!/bin/sh") (string-append "#!" bash "/bin/sh")))))
         %standard-phases))
       #:make-flags '("INSTALL_ARG=-no_chown")
       ;; No 'check' target.
       #:tests? #f))
    (home-page "http://www.exim.org/")
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
    (version "2.2.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.dovecot.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.gz"))
       (sha256 (base32
                "1w6gg4h9mxg3i8faqpmgj19imzyy001b0v8ihch8ma3zl63i5kjn"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("sqlite" ,sqlite)))
    (arguments
     `(#:configure-flags '("--sysconfdir=/etc"
                           "--localstatedir=/var")
       #:phases (modify-phases %standard-phases
                  (add-before
                   'configure 'pre-configure
                   (lambda _
                     ;; Simple hack to avoid installing in /etc
                     (substitute* '("doc/Makefile.in"
                                    "doc/example-config/Makefile.in")
                       (("pkgsysconfdir = .*")
                        "pkgsysconfdir = /tmp/etc"))
                     #t)))))
    (home-page "http://www.dovecot.org")
    (synopsis "Secure POP3/IMAP server")
    (description
     "Dovecot is a mail server whose major goals are security and reliability.
It supports mbox/Maildir and its own dbox/mdbox formats.")
    ;; Most source files are covered by either lgpl2.1 or expat.  The SHA code
    ;; is covered by a variant of BSD-3, and UnicodeData.txt is covered by the
    ;; Unicode, Inc. License Agreement for Data Files and Software.
    (license (list lgpl2.1 license:expat (non-copyleft "file://COPYING")))))

(define-public isync
  (package
    (name "isync")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/isync/isync/"
                           version "/isync-" version ".tar.gz"))
       (sha256 (base32
                "1960ah3fmp75cakd06lcx50n5q0yvfsadjh3lffhyvjvj7ava9d2"))))
    (build-system gnu-build-system)
    (inputs
     `(("bdb" ,bdb)
       ("openssl" ,openssl)))
    (home-page "http://isync.sourceforge.net/")
    (synopsis "Mailbox synchronization program")
    (description
     "isync/mbsync is command line tool for two-way synchronization of
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
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "http://search.cpan.org/dist/Email-Abstract")
    (synopsis "Interface to mail representations")
    (description "Email::Abstract provides module writers with the ability to
write simple, representation-independent mail handling code.")
    (license (package-license perl))))

(define-public perl-email-address
  (package
    (name "perl-email-address")
    (version "1.907")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Address-" version ".tar.gz"))
       (sha256
        (base32
         "1ai4r149pzjv9dc2vddir8zylj0z1pii93rm4g591lx7avim71hx"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Email-Address")
    (synopsis "Email address parsing and creation")
    (description "Email::Address implements a regex-based RFC 2822 parser that
locates email addresses in strings and returns a list of Email::Address
objects found.  Alternatively you may construct objects manually.")
    (license (package-license perl))))

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
    (home-page "http://search.cpan.org/dist/Email-Date-Format")
    (synopsis "Produce RFC 2822 date strings")
    (description "Email::Date::Format provides a means for generating an RFC
2822 compliant datetime string.")
    (license (package-license perl))))

(define-public perl-email-messageid
  (package
    (name "perl-email-messageid")
    (version "1.405")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MessageID-" version ".tar.gz"))
       (sha256
        (base32
         "09216naz21x99ff33wdm3j3zq1zhdbxhrsmx8bvavjrw3gjsvrq3"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Email-MessageID")
    (synopsis "Generate world unique message-ids")
    (description "Email::MessageID generates recommended message-ids to
identify a message uniquely.")
    (license (package-license perl))))

(define-public perl-email-mime
  (package
    (name "perl-email-mime")
    (version "1.929")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MIME-" version ".tar.gz"))
       (sha256
        (base32
         "1sf7dldg4dvicyw6dl1vx6s1gjq3fcppi0103ikl0vi6v5xjdjdh"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-email-address" ,perl-email-address)
       ("perl-email-messageid" ,perl-email-messageid)
       ("perl-email-mime-contenttype" ,perl-email-mime-contenttype)
       ("perl-email-mime-encodings" ,perl-email-mime-encodings)
       ("perl-email-simple" ,perl-email-simple)
       ("perl-mime-types" ,perl-mime-types)))
    (home-page "http://search.cpan.org/dist/Email-MIME")
    (synopsis "MIME message handling")
    (description "Email::MIME is an extension of the Email::Simple module, to
handle MIME encoded messages.  It takes a message as a string, splits it up
into its constituent parts, and allows you access to various parts of the
message.  Headers are decoded from MIME encoding.")
    (license (package-license perl))))

(define-public perl-email-mime-contenttype
  (package
    (name "perl-email-mime-contenttype")
    (version "1.017")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MIME-ContentType-" version ".tar.gz"))
       (sha256
        (base32
         "1cl1l97lg690dh7i704hqi7yhxalq1chy7ylld5yc5v38jqa6gcn"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page "http://search.cpan.org/dist/Email-MIME-ContentType")
    (synopsis "Parse MIME Content-Type headers")
    (description "Email::MIME::ContentType parses a MIME Content-Type
header.")
    (license (package-license perl))))

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
    (home-page "http://search.cpan.org/dist/Email-MIME-Encodings")
    (synopsis "Unified interface to MIME encoding and decoding")
    (description "This module wraps MIME::Base64 and MIME::QuotedPrint.")
    (license (package-license perl))))

(define-public perl-email-sender
  (package
    (name "perl-email-sender")
    (version "1.300016")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Sender-" version ".tar.gz"))
       (sha256
        (base32
         "18x26fjh399q3s2g8dajb9r10633c46jrnbvycpnpclgnzhjs100"))))
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
    (home-page "http://search.cpan.org/dist/Email-Sender")
    (synopsis "Perl library for sending email")
    (description "Email::Sender replaces the old and sometimes problematic
Email::Send library.")
    (license (package-license perl))))

(define-public perl-email-simple
  (package
    (name "perl-email-simple")
    (version "2.206")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "19dpy3j5na2k9qw1jcpc8ia25038068r9j1bn34f9yyrisz7s522"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-email-date-format" ,perl-email-date-format)))
    (home-page "http://search.cpan.org/dist/Email-Simple")
    (synopsis "Parsing of RFC 2822 messages")
    (description "Email::Simple provides simple parsing of RFC 2822 message
format and headers.")
    (license (package-license perl))))

;;; mail.scm ends here
