;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages networking)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages tls))

(define-public miredo
  (package
    (name "miredo")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.remlab.net/files/miredo/miredo-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0j9ilig570snbmj48230hf7ms8kvcwi2wblycqrmhh85lksd49ps"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; The checkconf test in src/ requires network access.
         (add-before
          'check 'disable-checkconf-test
          (lambda _
            (substitute* "src/Makefile"
              (("^TESTS = .*") "TESTS = \n")))))))
    (home-page "http://www.remlab.net/miredo/")
    (synopsis "Teredo IPv6 tunneling software")
    (description
     "Miredo is an implementation (client, relay, server) of the Teredo
specification, which provides IPv6 Internet connectivity to IPv6 enabled hosts
residing in IPv4-only networks, even when they are behind a NAT device.")
    (license license:gpl2+)))

(define-public socat
  (package
    (name "socat")
    (version "1.7.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.dest-unreach.org/socat/download/socat-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "028yjka2zr6j1i8pmfmvzqki8ajczdl1hnry1x31xbbg3j83jxsb"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))                    ;no 'check' phase
    (inputs `(("openssl" ,openssl)))
    (home-page "http://www.dest-unreach.org/socat/")
    (synopsis
     "Open bidirectional communication channels from the command line")
    (description
     "socat is a relay for bidirectional data transfer between two independent
data channels---files, pipes, devices, sockets, etc.  It can create
\"listening\" sockets, named pipes, and pseudo terminals.

socat can be used, for instance, as TCP port forwarder, as a shell interface
to UNIX sockets, IPv6 relay, for redirecting TCP oriented programs to a serial
line, to logically connect serial lines on different computers, or to
establish a relatively secure environment (su and chroot) for running client
or server shell scripts with network connections. ")
    (license license:gpl2)))

(define-public zeromq
  (package
    (name "zeromq")
    (version "4.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.zeromq.org/zeromq-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0arl8fy8d03xd5h0mgda1s5bajwg8iyh1kk4hd1420rpcxgkrj9v"))))
    (build-system gnu-build-system)
    (home-page "http://zeromq.org")
    (synopsis "Library for message-based applications")
    (description
     "The 0MQ lightweight messaging kernel is a library which extends the
standard socket interfaces with features traditionally provided by specialized
messaging middle-ware products.  0MQ sockets provide an abstraction of
asynchronous message queues, multiple messaging patterns, message
filtering (subscriptions), seamless access to multiple transport protocols and
more.")
    (license license:lgpl3+)))
