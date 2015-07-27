;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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
  #:use-module (gnu packages admin)
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
or server shell scripts with network connections.")
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

(define-public libndp
  (package
    (name "libndp")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://libndp.org/files/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "15f743hjc7yy2sv3hzvfc27s1gny4mh5aww59vn195fff2midwgs"))))
    (build-system gnu-build-system)
    (home-page "http://libndp.org/")
    (synopsis "Library for Neighbor Discovery Protocol")
    (description
     "libndp contains a library which provides a wrapper for IPv6 Neighbor
Discovery Protocol.  It also provides a tool named ndptool for sending and
receiving NDP messages.")
    (license license:lgpl2.1+)))

(define-public ethtool
  (package
    (name "ethtool")
    (version "4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/network/"
                                  name "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1zzcwn6pk8qfasalqkxg8vrhacksfa50xsq4xifw7yfjqyn8fj4h"))))
    (build-system gnu-build-system)
    (home-page "https://www.kernel.org/pub/software/network/ethtool/")
    (synopsis "Display or change Ethernet device settings")
    (description
     "ethtool can be used to query and change settings such as speed,
auto-negotiation and checksum offload on many network devices, especially
Ethernet devices.")
    (license license:gpl2)))

(define-public ppp
  (package
    (name "ppp")
    (version "2.4.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.samba.org/pub/" name "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c7vrjxl52pdwi4ckrvfjr08b31lfpgwf3pp0cqy76a77vfs7q02"))))
    (build-system gnu-build-system)
    (inputs
     `(("libpcap" ,libpcap)))
    (arguments
     `(#:tests? #f  ; no test suite
       #:make-flags '("CC=gcc")
       #:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'patch-pppd-makefile
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "pppd/Makefile.linux"
                       (("/usr/include/crypt.h")
                        (string-append (assoc-ref inputs "libc")
                                       "/include/crypt.h")))
                     #t)))))
    (home-page "https://ppp.samba.org/")
    (synopsis "Point-to-Point Protocol (PPP)")
    (description
     "The Point-to-Point Protocol provides a standard way to transmit
datagrams over a serial link, as well as a standard way for the machines at
either end of the link to negotiate various optional characteristics of the
link.")
    ;; XXX TODO Document all of the licenses.
    (license (license:non-copyleft
              "http://metadata.ftp-master.debian.org/changelogs//main/p/ppp/ppp_2.4.6-3.1_copyright"))))
