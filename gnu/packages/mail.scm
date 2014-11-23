;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2014 Sou Bunnbu <iyzsong@gmail.com>
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
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openssl)
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
  #:use-module ((guix licenses)
                #:select (gpl2 gpl2+ gpl3+ lgpl2.1+ lgpl3+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
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
       ("libtool" ,libtool)
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
    (home-page "http://fetchmail.berlios.de/")
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
               "0dzx4qk50pjfsb6cs5jahng96a52k12f7pm0sc78iqdrawg71w1s"))))
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
          ;; The test programs run several programs using 'system'
          ;; with hard-coded paths.  Here we patch them all.  We also
          ;; change "gpg" to "gpg2".
          (substitute* (find-files "tests" "\\.c$")
            (("(system *\\(\")(/[^ ]*)" all pre prog-path)
             (let* ((base (basename prog-path))
                    (prog (which (if (string=? base "gpg") "gpg2" base))))
              (string-append pre (or prog (error "not found: " base)))))))
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
    (version "0.9.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mu0.googlecode.com/files/mu-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1hwkliyb8fjrz5sw9fcisssig0jkdxzhccw0ld0l9a10q1l9mqhp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")             ; for gtester
       ("texinfo" ,texinfo)))
    ;; TODO: Add webkit and gtk to build the mug GUI.
    (inputs
     `(("xapian" ,xapian)
       ("emacs" ,emacs)
       ("guile" ,guile-2.0)
       ("glib" ,glib)
       ("gmime" ,gmime)
       ("tzdata" ,tzdata)))             ;for mu/test/test-mu-query.c
    (arguments
     '(#:phases (alist-cons-before
                 'check 'check-tz-setup
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; For mu/test/test-mu-query.c
                   (setenv "TZDIR"
                           (string-append (assoc-ref inputs "tzdata")
                                          "/share/zoneinfo")))
                 %standard-phases)))
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
    (version "0.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://notmuchmail.org/releases/notmuch-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1pdp9l7yv71d3fjb30qyccva8h03hvg88q4a00yi50v2j70kvmgj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;; FIXME: Test suite hangs and times out.
       #:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (setenv "CONFIG_SHELL" (which "sh"))
                   (let ((out (assoc-ref outputs "out")))
                     (zero? (system* "./configure"
                                     (string-append "--prefix=" out)))))
                 %standard-phases)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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

;;; mail.scm ends here
