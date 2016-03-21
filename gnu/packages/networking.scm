;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2016 Raimon Grau <raimonster@gmail.com>
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
  #:use-module (gnu packages tls)
  #:use-module (gnu packages ncurses))

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
    (version "1.7.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.dest-unreach.org/socat/download/socat-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1apvi7sahcl44arnq1ad2y6lbfqnmvx7nhz9i3rkk0f382anbnnj"))))
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

(define-public ifstatus
  (package
    (name "ifstatus")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ifstatus/ifstatus-v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "045cbsq9ps32j24v8y5hpyqxnqn9mpaf3mgvirlhgpqyb9jsia0c"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "Main.h"
                  (("#include <stdio.h>")
                   "#include <stdio.h>\n#include <stdlib.h>")))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                                ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)             ; no configure script
         (replace 'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (bin (string-append out "/bin")))
                      (mkdir-p bin)
                      (copy-file "ifstatus"
                                 (string-append bin "/ifstatus"))))))))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://ifstatus.sourceforge.net/graphic/index.html")
    (synopsis "Text based network interface status monitor")
    (description
     "IFStatus is a simple, easy-to-use program for displaying commonly
needed/wanted real-time traffic statistics of multiple network
interfaces, with a simple and efficient view on the command line.  It is
intended as a substitute for the PPPStatus and EthStatus projects.")
    (license license:gpl2+)))

(define-public nload
  (package
    (name "nload")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/nload/nload-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1rb9skch2kgqzigf19x8bzk211jdfjfdkrcvaqyj89jy2pkm3h61"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://www.roland-riegel.de/nload/")
    (synopsis "Realtime console network usage monitor")
    (description
     "Nload is a console application which monitors network traffic and
bandwidth usage in real time.  It visualizes the in- and outgoing traffic using
two graphs and provides additional info like total amount of transfered data
and min/max network usage.")
    (license license:gpl2+)))
