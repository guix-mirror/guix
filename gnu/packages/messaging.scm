;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
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

(define-module (gnu packages messaging)
  #:use-module ((guix licenses)
                #:select (gpl2+ gpl2 lgpl2.1 bsd-2))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tls))

(define-public libotr
  (package
    (name "libotr")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://otr.cypherpunks.ca/libotr-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0c6rkh58s6wqzcrpccwdik5qs91qj6dgd60a340d72gc80cqknsg"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgcrypt" ,libgcrypt)))  ; libotr headers include gcrypt.h
    (inputs `(("libgpg-error" ,libgpg-error)))
    (synopsis "Off-the-Record (OTR) Messaging Library and Toolkit")
    (description
     "OTR allows you to have private conversations over instant messaging by
providing:
* Encryption: No one else can read your instant messages.
* Authentication: You are assured the correspondent is who you think it is.
* Deniability: The messages you send do not have digital signatures that are
  checkable by a third party.  Anyone can forge messages after a conversation
  to make them look like they came from you.  However, during a conversation,
  your correspondent is assured the messages he sees are authentic and
  unmodified.
* Perfect forward secrecy: If you lose control of your private keys, no
  previous conversation is compromised.")
    (home-page "https://otr.cypherpunks.ca/")
    (license (list lgpl2.1 gpl2))))

(define-public libotr-3
  (package (inherit libotr)
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://otr.cypherpunks.ca/libotr-"
                                  version ".tar.gz"))
              (sha256
               (base32 "1x6dd4rh499hdraiqfhz81igrj0a5rs0gjhc8l4sljwqhjjyla6l"))))))

(define-public bitlbee
  (package
    (name "bitlbee")
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://get.bitlbee.org/src/bitlbee-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0plx4dryf8i6hz7vghg84z5f6w6rkw1l8ckl4c4wh5zxpd3ddfnf"))
              (patches (list (search-patch "bitlbee-configure-doc-fix.patch")))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("check" ,check)))
    (inputs `(("glib" ,glib)
              ("libotr" ,libotr)
              ("gnutls" ,gnutls)
              ("python" ,python-2)
              ("perl" ,perl)))
    (arguments
     `(#:phases (alist-cons-after
                 'install 'install-etc
                 (lambda* (#:key (make-flags '()) #:allow-other-keys)
                   (zero? (apply system* "make" "install-etc" make-flags)))
                 (alist-replace
                  'configure
                  ;; bitlbee's configure script does not tolerate many of the
                  ;; variable settings that Guix would pass to it.
                  (lambda* (#:key outputs #:allow-other-keys)
                    (zero? (system* "./configure"
                                    (string-append "--prefix="
                                                   (assoc-ref outputs "out"))
                                    "--otr=1")))
                  %standard-phases))))
    (synopsis "IRC to instant messaging gateway")
    (description "BitlBee brings IM (instant messaging) to IRC clients, for
people who have an IRC client running all the time and don't want to run an
additional IM client.  BitlBee currently supports XMPP/Jabber (including
Google Talk), MSN Messenger, Yahoo!  Messenger, AIM and ICQ, and the Twitter
microblogging network (plus all other Twitter API compatible services like
identi.ca and status.net).")
    (home-page "http://www.bitlbee.org/")
    (license (list gpl2+ bsd-2))))

(define-public hexchat
  (package
    (name "hexchat")
    (version "2.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.hexchat.net/hexchat/hexchat-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1ag9rmfisv0hsbk05jq4f1rnap7kwg90vgbmkr9zklkh6imfxk7z"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("intltool" ,intltool)))
    (inputs `(("dbus-glib" ,dbus-glib)
              ("dbus" ,dbus)
              ("enchant" ,enchant)
              ("glib:bin" ,glib "bin")            ;need glib-genmarshal
              ("gtk" ,gtk+-2)
              ("libcanberra" ,libcanberra)
              ("libnotify" ,libnotify)
              ("openssl" ,openssl)
              ("perl-xml-parser" ,perl-xml-parser) ;for addons
              ("python-2" ,python-2)))             ;for addons
    (synopsis "Graphical IRC Client")
    (description
     "HexChat lets you connect to multiple IRC networks at once.  The main window
shows the list of currently connected networks and their channels, the current
conversation and the list of users.  It uses colors to differentiate between
users and to highlight messages.  It checks spelling using available
dictionaries.  HexChat can be extended with multiple addons.")
    (home-page "http://hexchat.net/")
    (license gpl2+)))

(define-public ngircd
  (package
    (name "ngircd")
    (version "22")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://arthur.barton.de/pub/ngircd/ngircd-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "17k3g9qd9d010czk5846qxvzkmw4fihv8l6m2a2287crbxm3xhd4"))
              (patches (list (search-patch "ngircd-no-dns-in-tests.patch")
                             (search-patch "ngircd-handle-zombies.patch")))))
    (build-system gnu-build-system)
    ;; Needed for the test suite.
    (native-inputs `(("procps" ,procps)
                     ("expect" ,expect)
                     ("inetutils" ,inetutils)))
    ;; XXX Add libident, libwrap.
    (inputs `(("zlib" ,zlib)
              ("gnutls" ,gnutls)
              ,@(if (string-suffix? "-linux"
                                    (or (%current-target-system)
                                        (%current-system)))
                    `(("linux-pam" ,linux-pam))
                    '())))
    (arguments
     `(#:configure-flags
       '("--with-gnutls" "--with-iconv" "--enable-ipv6"
         ,@(if (string-suffix? "-linux"
                               (or (%current-target-system)
                                   (%current-system)))
               '("--with-pam")
               '()))
       #:phases
       ;; Necessary for the test suite.
       (alist-cons-after
        'configure 'post-configure
        (lambda _
          (substitute* "src/ngircd/Makefile"
            (("/bin/sh") (which "sh")))
          ;; The default getpid.sh does a sloppy grep over 'ps -ax' output,
          ;; which fails arbitrarily.
          (with-output-to-file "src/testsuite/getpid.sh"
            (lambda ()
              (display
               (string-append
                "#!" (which "sh") "\n"
                "ps -C \"$1\" -o pid=\n"))))
          ;; Our variant of getpid.sh does not work for interpreter names if a
          ;; shebang script is run directly as "./foo", so patch cases where
          ;; the test suite relies on this.
          (substitute* "src/testsuite/start-server.sh"
            ;; It runs 'getpid.sh sh' to test if it works at all.  Run it on
            ;; 'make' instead.
            (("getpid.sh sh") "getpid.sh make")))
        %standard-phases)))
    (home-page "http://ngircd.barton.de/")
    (synopsis "Lightweight Internet Relay Chat server for small networks")
    (description
     "ngIRCd is a lightweight Internet Relay Chat server for small or private
networks.  It is easy to configure, can cope with dynamic IP addresses, and
supports IPv6, SSL-protected connections as well as PAM for authentication.")
    (license gpl2+)))

;;; messaging.scm ends here
