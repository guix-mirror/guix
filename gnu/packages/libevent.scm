;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages libevent)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config))

(define-public libevent
  (package
    (name "libevent")
    (version "2.1.8")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/libevent/libevent/releases/download/release-"
                   version "-stable/libevent-" version "-stable.tar.gz"))
             (sha256
              (base32
               "1hhxnxlr0fsdv7bdmzsnhdz16fxf3jg2r6vyljcl3kj6pflcap4n"))
             (patches (search-patches "libevent-2.1-dns-tests.patch"
                                      ;; XXX: Try removing this for > 2.1.8.
                                      ;; https://github.com/libevent/libevent/issues/452
                                      "libevent-2.1-skip-failing-test.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("python" ,python-2)))           ; for 'event_rpcgen.py'
    (native-inputs
     `(("which" ,which)))
    (home-page "http://libevent.org/")
    (synopsis "Event notification library")
    (description
     "The libevent API provides a mechanism to execute a callback
function when a specific event occurs on a file descriptor or after a
timeout has been reached.  Furthermore, libevent also support callbacks
due to signals or regular timeouts.

libevent is meant to replace the event loop found in event driven
network servers.  An application just needs to call event_dispatch() and
then add or remove events dynamically without having to change the event
loop.")
    (license bsd-3)))

(define-public libevent-2.0
  (package
    (inherit libevent)
    (version "2.0.22")
    (source (origin
          (method url-fetch)
          (uri (string-append
                "https://github.com/libevent/libevent/releases/download/release-"
                version "-stable/libevent-" version "-stable.tar.gz"))
          (sha256
           (base32
            "18qz9qfwrkakmazdlwxvjmw8p76g70n3faikwvdwznns1agw9hki"))
          (patches (search-patches
                    "libevent-dns-tests.patch"
                    "libevent-2.0-CVE-2016-10195.patch"
                    "libevent-2.0-CVE-2016-10196.patch"
                    "libevent-2.0-CVE-2016-10197.patch"))))))

(define-public libev
  (package
    (name "libev")
    (version "4.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dist.schmorp.de/libev/Attic/libev-"
                                  version
                                  ".tar.gz"))
              (sha256
               (base32
                "0ynxxm7giy4hg3qp9q8wshqw1jla9sxbsbi2pwsdsl1v1hz79zn7"))))
    (build-system gnu-build-system)
    (home-page "http://software.schmorp.de/pkg/libev.html")
    (synopsis "Event loop loosely modelled after libevent")
    (description
     "libev is a full-featured and high-performance event loop that
is loosely modelled after libevent, but without its limitations and
bugs.  It is used in GNU Virtual Private Ethernet, rxvt-unicode,
auditd, the Deliantra MORPG Server and Client, and many other
programs.")
    (license
     (list bsd-2 gpl2+))))

(define-public libuv
  (package
    (name "libuv")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libuv/libuv/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1sx5lahhg2w92y6mgyg7c7nrx2biyyxd5yiqkmq8n4w01lm2gf7q"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-after
                 'unpack 'autogen
                 (lambda _
                   ;; Fashionable people don't run 'make dist' these days, so
                   ;; we need to do that ourselves.
                   (zero? (system* "sh" "autogen.sh")))
                 %standard-phases)

       ;; XXX: Some tests want /dev/tty, attempt to make connections, etc.
       #:tests? #f))
    (native-inputs `(("autoconf" ,(autoconf-wrapper))
                     ("automake" ,automake)
                     ("libtool" ,libtool)

                     ;; libuv.pc is installed only when pkg-config is found.
                     ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/joyent/libuv")
    (synopsis "Library for asynchronous I/O")
    (description
     "libuv is a multi-platform support library with a focus on asynchronous
I/O.  Among other things, it supports event loops via epoll, kqueue, and
similar IOCP, and event ports, asynchronous TCP/UDP sockets, asynchronous DNS
resolution, asynchronous file system operations, and threading primitives.")

    ;; A few files fall under other non-copyleft licenses; see 'LICENSE' for
    ;; details.
    (license x11)))
