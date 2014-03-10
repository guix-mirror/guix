;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check))

(define-public libotr
  (package
    (name "libotr")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://otr.cypherpunks.ca/libotr-"
                                  version ".tar.gz"))
              (sha256
               (base32 "1d4k0b7v4d3scwm858cmqr9c6xgd6ppla1vk4x2yg64q82a1k49z"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgcrypt" ,libgcrypt)))  ; libotr headers include gcrypt.h
    (inputs `(("libgpg-error" ,libgpg-error)))
    (arguments
     `(#:configure-flags '("--with-pic")))
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
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://get.bitlbee.org/src/bitlbee-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0n8g5452i5qap43zxb83gxp01d48psf6rr3k1q7z6a3dgpfi3x00"))
              (patches (list (search-patch "bitlbee-memset-fix.patch")
                             (search-patch "bitlbee-fix-tests.patch")))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("check" ,check)))
    (inputs `(("glib" ,glib)
              ("libotr" ,libotr-3)
              ("gnutls" ,gnutls)
              ("zlib" ,zlib)  ; Needed to satisfy "pkg-config --exists gnutls"
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

;;; messaging.scm ends here
