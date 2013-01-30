;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages ssh)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages openssl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libssh2
  (package
   (name "libssh2")
   (version "1.4.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "http://www.libssh2.org/download/libssh2-"
                   version ".tar.gz"))
            (sha256 (base32
                     "0vdr478dbhbdgnniqmirawjb7mrcxckn4slhhrijxnzrkmgziipa"))))
   (build-system gnu-build-system)
   (inputs `(("libgcrypt" ,libgcrypt)
             ("zlib" ,zlib)))
   (synopsis "libssh2, a client-side C library implementing the SSH2 protocol")
   (description
    "libssh2 is a library intended to allow software developers access to
the SSH-2 protocol in an easy-to-use self-contained package. It can be built
into an application to perform many different tasks when communicating with
a server that supports the SSH-2 protocol.")
   (license license:bsd-3)
   (home-page "http://www.libssh2.org/")))

(define-public openssh
  (package
   (name "openssh")
   (version "6.1p1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "ftp://ftp.fr.openbsd.org/pub/OpenBSD/OpenSSH/portable/openssh-"
                   version ".tar.gz"))
            (sha256 (base32
                     "04f4l4vx6f964v5qjm03nhyixdc3llc90z6cj70r0bl5q3v5ghfi"))))
   (build-system gnu-build-system)
   (inputs `(("groff" ,groff)
             ("openssl" ,openssl)
             ("zlib" ,zlib)))
   (arguments
    `(#:test-target "tests"
      #:phases
       (alist-replace
        'configure
        (lambda* (#:key outputs #:allow-other-keys #:rest args)
         (let ((configure (assoc-ref %standard-phases 'configure))
               (out (assoc-ref outputs "out")))
           (apply configure args)
           (substitute* "Makefile"
                        (("PRIVSEP_PATH=/var/empty")
                        (string-append "PRIVSEP_PATH=" out "/var/empty")))))
       (alist-replace
        'check
        (lambda* (#:key #:allow-other-keys #:rest args)
         (let ((check (assoc-ref %standard-phases 'check)))
           ;; remove tests that require the user sshd
           (substitute* "regress/Makefile"
                        (("t9 t-exec") "t9"))
           (apply check args)))
       (alist-replace
        'install
        (lambda* (#:key (make-flags '()) #:allow-other-keys)
          ;; install without host keys and system configuration files
          (zero? (apply system* "make" "install-nosysconf" make-flags)))
       %standard-phases)))))
   (synopsis "OpenSSH, a client and server for the secure shell (ssh) protocol")
   (description
    "The SSH2 protocol implemented in OpenSSH is standardised by the
IETF secsh working group and is specified in several RFCs and drafts.
It is composed of three layered components:

The transport layer provides algorithm negotiation and a key exchange.
The key exchange includes server authentication and results in a
cryptographically secured connection: it provides integrity, confidentiality
and optional compression.

The user authentication layer uses the established connection and relies on
the services provided by the transport layer. It provides several mechanisms
for user authentication. These include traditional password authentication
as well as public-key or host-based authentication mechanisms.

The connection layer multiplexes many different concurrent channels over the
authenticated connection and allows tunneling of login sessions and
TCP-forwarding. It provides a flow control service for these channels.
Additionally, various channel-specific options can be negotiated.")
   (license (license:bsd-style "file://LICENSE"
                               "See LICENSE in the distribution."))
   (home-page "http://www.openssh.org/")))

