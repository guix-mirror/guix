;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2016 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2016, 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2017 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017 ng0 <ng0@infotropique.org>
;;; Copyright © 2016, 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016 Benz Schenk <benz.schenk@uzh.ch>
;;; Copyright © 2016, 2017 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match))

(define-public macchanger
  (package
    (name "macchanger")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/"
                                  name "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xsiivjjyhqcs6dyjcshrnxlgypvyfzacjz7gcjgl88xiw9lylri"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/macchanger/")
    (synopsis "Viewing and manipulating MAC addresses of network interfaces")
    (description "GNU MAC Changer is a utility for viewing and changing MAC
addresses of networking devices.  New addresses may be set explicitly or
randomly.  They can include MAC addresses of the same or other hardware vendors
or, more generally, MAC addresses of the same category of hardware.")
    (license license:gpl2+)))

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

(define-public tcp-wrappers
  (package
    (name "tcp-wrappers")
    (version "7.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.porcupine.org/pub/security/tcp_wrappers_"
                    version ".tar.gz"))
              (sha256
               (base32
                "0p9ilj4v96q32klavx0phw9va21fjp8vpk11nbh6v2ppxnnxfhwm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)  ; there is no configure script
         (delete 'check)      ; there are no tests
         (replace 'build
           (lambda _
             (chmod "." #o755)
             ;; Upstream doesn't generate a shared library.  So we have to do it.
             (setenv "CC" "gcc -fno-builtin -fPIC")
             (substitute* "Makefile"
               (("^(all[^\n]*)" line) (string-append line " libwrap.so\n
libwrap.so: $(LIB_OBJ)\n
\tgcc -shared $^ -o $@\n")))
             ;; Deal with some gcc breakage.
             (substitute* "percent_m.c"
               (("extern char .sys_errlist.*;") ""))
             (substitute* "scaffold.c"
               (("extern char .malloc.*;") ""))
             ;; This, believe it or not, is the recommended way to build!
             (zero? (system* "make" "REAL_DAEMON_DIR=/etc" "linux"))))
         ;; There is no make install stage, so we have to do it ourselves.
         (replace 'install
           (lambda _
             (let ((out (assoc-ref %outputs "out"))
                   (man-pages `("hosts_access.3"
                                "hosts_access.5"
                                "hosts_options.5"
                                "tcpd.8"
                                "tcpdchk.8"
                                "tcpdmatch.8"))
                   (libs  `("libwrap.a"
                            "libwrap.so"))
                   (headers `("tcpd.h"))
                   (bins `("safe_finger"
                           "tcpd"
                           "tcpdchk"
                           "tcpdmatch"
                           "try-from")))
               (for-each
                (lambda (x)
                  (install-file x (string-append out "/include")))
                headers)
               (for-each
                (lambda (x)
                  (install-file x (string-append out "/share/man/man"
                                                 (string-take-right x 1))))
                man-pages)
               (for-each
                (lambda (x)
                  (install-file x (string-append out "/lib/")))
                libs)
               (for-each
                (lambda (x)
                  (install-file x (string-append out "/bin/")))
                bins)))))))
    (home-page "http://www.porcupine.org")
    (synopsis  "Monitor and filter incoming requests for network services")
    (description "With this package you can monitor and filter incoming requests for
network services.  It includes a library which may be used by daemons to
transparently check connection attempts against an access control list.")
    (license (license:non-copyleft "file://DISCLAIMER"
                                   "See the file DISCLAIMER in the distribution."))))


(define-public zeromq
  (package
    (name "zeromq")
    (version "4.0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.zeromq.org/zeromq-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00vvwhgcdr1lva2pavicvy92iad0hj8cf71n702hv6blw1kjj2z0"))))
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

(define-public librdkafka
  (package
    (name "librdkafka")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/edenhill/librdkafka/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "10ldx7g7ymmg17snzx78vy4n8ma1rjx0agzi34g15j2fk867xmas"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; its custom configure script doesn't understand 'CONFIG_SHELL'.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; librdkafka++.so lacks RUNPATH for librdkafka.so
               (setenv "LDFLAGS"
                       (string-append "-Wl,-rpath=" out "/lib"))
               (zero? (system* "./configure"
                               (string-append "--prefix=" out)))))))))
    (native-inputs
     `(("python" ,python-wrapper)))
    (propagated-inputs
     `(("zlib" ,zlib))) ; in the Libs.private field of rdkafka.pc
    (home-page "https://github.com/edenhill/librdkafka")
    (synopsis "Apache Kafka C/C++ client library")
    (description
     "librdkafka is a C library implementation of the Apache Kafka protocol,
containing both Producer and Consumer support.")
    (license license:bsd-2)))

(define-public libndp
  (package
    (name "libndp")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://libndp.org/files/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "03mczwrxqbp54msafxzzyhaazkvjdwm2kipjkrb5xg8kw22glz8c"))))
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
    (version "4.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/network/"
                                  name "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1flwz4x76ajxigadq9knxgwr778g03y3qfx6c7rflc3x020a7hdp"))))
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
              (uri (string-append "mirror://sourceforge/ifstatus/ifstatus/"
                                  "ifstatus%20v" version "/ifstatus-v"
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
              (uri (string-append "mirror://sourceforge/nload/nload/" version
                                  "/nload-" version ".tar.gz"))
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
two graphs, and provides additional info like total amount of transferred data
and min/max network usage.")
    (license license:gpl2+)))

(define-public iodine
  (package
    (name "iodine")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://code.kryo.se/" name "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gh17kcxxi37k65zm4gqsvbk3aw7yphcs3c02pn1c4s2y6n40axd"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-ifconfig-path
           ;; This package works only with the net-tools version of ifconfig.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/tun.c"
               (("PATH=[^ ]* ")
                (string-append (assoc-ref inputs "net-tools") "/bin/")))))
         (add-before 'check 'delete-failing-tests
           ;; Avoid https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=802105.
           (lambda _
             (substitute* "tests/common.c"
               (("tcase_add_test\\(tc, \
test_parse_format_ipv(4(|_listen_all|_mapped_ipv6)|6)\\);")
                "")))))
       #:make-flags (list "CC=gcc"
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:test-target "test"))
    (inputs `(("net-tools" ,net-tools)
              ("zlib" ,zlib)))
    (native-inputs `(("check" ,check)
                     ("pkg-config" ,pkg-config)))
    (home-page "http://code.kryo.se/iodine/")
    (synopsis "Tunnel IPv4 data through a DNS server")
    (description "Iodine tunnels IPv4 data through a DNS server.  This
can be useful in different situations where internet access is firewalled, but
DNS queries are allowed.  The bandwidth is asymmetrical, with limited upstream
and up to 1 Mbit/s downstream.")
    ;; src/md5.[ch] is released under the zlib license
    (license (list license:isc license:zlib))))

(define-public whois
  (package
    (name "whois")
    (version "5.2.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://debian/pool/main/w/whois/"
                           name "_" version ".tar.xz"))
       (sha256
        (base32
         "0b16w48c17k35lhd95qcl2kjq2rahk8znkg3w467rf3kzmsa4fbc"))))
    (build-system gnu-build-system)
    ;; TODO: unbundle mkpasswd binary + its po files.
    (arguments
     `(#:tests? #f ; Does not exist
       #:make-flags (list "CC=gcc"
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No configure
         (add-before 'build 'setenv
           (lambda _
             (setenv "HAVE_ICONV" "1")
             #t)))))
    (inputs
     ;; TODO: Switch to libidn2 when >= 2.0.3 is ungrafted in master.
     `(("libidn" ,libidn)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (synopsis "Improved whois client")
    (description "This whois client is intelligent and can
automatically select the appropriate whois server for most queries.
Because of historical reasons this also includes a tool called mkpasswd
which can be used to encrypt a password with @code{crypt(3)}.")
    (home-page "https://github.com/rfc1036/whois")
    (license license:gpl2+)))

(define-public wireshark
  (package
    (name "wireshark")
    (version "2.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.wireshark.org/download/src/wireshark-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0bpiby916k3k8bm7q8b1dflva6zs0a4ircskrck0d538dfcrb50q"))))
    (build-system gnu-build-system)
    (inputs `(("c-ares" ,c-ares)
              ("glib" ,glib)
              ("gnutls" ,gnutls)
              ("libcap" ,libcap)
              ("libgcrypt" ,libgcrypt)
              ("libnl" ,libnl)
              ("libpcap" ,libpcap)
              ("libssh" ,libssh)
              ("libxml2" ,libxml2)
              ("lua" ,lua-5.2)          ;Lua 5.3 unsupported
              ("krb5" ,mit-krb5)
              ("openssl" ,openssl)
              ("portaudio" ,portaudio)
              ("qtbase" ,qtbase)
              ("sbc" ,sbc)
              ("zlib" ,zlib)))
    (native-inputs `(("perl" ,perl)
                     ("pkg-config" ,pkg-config)
                     ("python" ,python-wrapper)
                     ("qttools" ,qttools)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-c-ares=" (assoc-ref %build-inputs "c-ares"))
             (string-append "--with-krb5=" (assoc-ref %build-inputs "krb5"))
             (string-append "--with-libcap=" (assoc-ref %build-inputs "libcap"))
             (string-append "--with-libssh=" (assoc-ref %build-inputs "libssh"))
             (string-append "--with-lua=" (assoc-ref %build-inputs "lua"))
             (string-append "--with-pcap=" (assoc-ref %build-inputs "libpcap"))
             (string-append "--with-portaudio="
                            (assoc-ref %build-inputs "portaudio"))
             (string-append "--with-sbc=" (assoc-ref %build-inputs "sbc"))
             (string-append "--with-ssl=" (assoc-ref %build-inputs "openssl"))
             (string-append "--with-zlib=" (assoc-ref %build-inputs "zlib")))))
    (synopsis "Network traffic analyzer")
    (description "Wireshark is a network protocol analyzer, or @dfn{packet
sniffer}, that lets you capture and interactively browse the contents of
network frames.")
    (home-page "https://www.wireshark.org/")
    (license license:gpl2+)))

(define-public fping
  (package
    (name "fping")
    (version "4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://fping.org/dist/fping-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1kp81wchi79l8z8rrj602fpjrd8bi84y3i7fsaclzlwap5943sv7"))))
    (build-system gnu-build-system)
    (home-page "http://fping.org/")
    (synopsis "Send ICMP ECHO_REQUEST packets to network hosts")
    (description
     "fping is a ping like program which uses the Internet Control Message
Protocol (ICMP) echo request to determine if a target host is responding.
fping differs from ping in that you can specify any number of targets on the
command line, or specify a file containing the lists of targets to ping.
Instead of sending to one target until it times out or replies, fping will
send out a ping packet and move on to the next target in a round-robin
fashion.")
    (license license:expat)))

(define-public httping
  (package
    (name "httping")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.vanheusden.com/httping/httping-"
                           version ".tgz"))
       (sha256
        (base32
         "1y7sbgkhgadmd93x1zafqc4yp26ssiv16ni5bbi9vmvvdl55m29y"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("fftw" ,fftw)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)))
    (arguments
     `(#:make-flags (list "CC=gcc"
                          (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:tests? #f)) ; no tests
    (home-page "https://www.vanheusden.com/httping/")
    (synopsis "Web server latency and throughput monitor")
    (description
     "httping measures how long it takes to connect to a web server, send an
HTTP(S) request, and receive the reply headers.  It is somewhat similar to
@command{ping}, but can be used even in cases where ICMP traffic is blocked
by firewalls or when you want to monitor the response time of the actual web
application stack itself.")
    (license license:gpl2)))        ; with permission to link with OpenSSL

(define-public httpstat
  (package
    (name "httpstat")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "httpstat" version))
       (sha256
        (base32
         "1chw2nk56vaq87aba012a270k9na06hfx1pfbsrc3jfvlc2kb9hb"))))
    (build-system python-build-system)
    (inputs `(("curl" ,curl)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-curl-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "httpstat.py"
               (("ENV_CURL_BIN.get\\('curl'\\)")
                (string-append "ENV_CURL_BIN.get('"
                               (assoc-ref inputs "curl")
                               "/bin/curl')"))))))))
    (home-page "https://github.com/reorx/httpstat")
    (synopsis "Visualize curl statistics")
    (description
     "@command{httpstat} is a tool to visualize statistics from the
@command{curl} HTTP client.  It acts as a wrapper for @command{curl} and
prints timing information for each step of the HTTP request (DNS lookup,
TCP connection, TLS handshake and so on) in the terminal.")
    (license license:expat)))

(define-public bwm-ng
  (package
    (name "bwm-ng")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gropp.org/bwm-ng/bwm-ng-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1w0dwpjjm9pqi613i8glxrgca3rdyqyp3xydzagzr5ndc34z6z02"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (synopsis "Console based live network and disk I/O bandwidth monitor")
    (description "Bandwidth Monitor NG is a small and simple console based
live network and disk I/O bandwidth monitor.")
    (home-page "https://www.gropp.org/?id=projects&sub=bwm-ng")
    (license license:gpl2)))

(define-public aircrack-ng
  (package
    (name "aircrack-ng")
    (version "1.2-rc4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.aircrack-ng.org/aircrack-ng-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0dpzx9kddxpgzmgvdpl3rxn0jdaqhm5wxxndp1xd7d75mmmc2fnr"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("libnl" ,libnl)
       ("ethtool" ,ethtool)
       ("pcre" ,pcre)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (arguments
     `(#:make-flags `("sqlite=true"
                      "gcrypt=true"
                      "libnl=true"
                      "pcre=true"
                      "experimental=true" ;build wesside-ng, etc.
                      "AVX2FLAG=N" "AVX1FLAG=N"
                      ,,@(match (%current-system)
                           ((or "x86_64-linux" "i686-linux")
                            `("SSEFLAG=Y"))
                           (_
                            `("NEWSSE=false")))
                      ,(string-append "prefix=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configure phase
                  (add-after 'build 'absolutize-tools
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((ethtool (string-append (assoc-ref inputs "ethtool")
                                                    "/sbin/ethtool")))
                        (substitute* "scripts/airmon-ng"
                          (("\\[ ! -x \"\\$\\(command -v ethtool 2>&1)\" \\]")
                           (string-append "! " ethtool " --version "
                                          ">/dev/null 2>&1"))
                          (("\\$\\(ethtool")
                           (string-append "$(" ethtool)))
                        #t))))))
    (home-page "http://www.aircrack-ng.org")
    (synopsis "Assess WiFi network security")
    (description
     "Aircrack-ng is a complete suite of tools to assess WiFi network
security.  It focuses on different areas of WiFi security: monitoring,
attacking, testing, and cracking.  All tools are command-line driven, which
allows for heavy scripting.")
    (license (list license:gpl2+ license:bsd-3))))

(define-public perl-net-dns
 (package
  (name "perl-net-dns")
  (version "1.14")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/N/NL/NLNETLABS/Net-DNS-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1z4r092qv0ify033dld5jayk8gs0bc7pl130dvb8ab7b9rcqmhw3"))))
  (build-system perl-build-system)
  (inputs
    `(("perl-digest-hmac" ,perl-digest-hmac)))
  (home-page "http://search.cpan.org/dist/Net-DNS")
  (synopsis
    "Perl Interface to the Domain Name System")
  (description "Net::DNS is the Perl Interface to the Domain Name System.")
  (license license:x11)))

(define-public perl-socket6
 (package
  (name "perl-socket6")
  (version "0.28")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/U/UM/UMEMOTO/Socket6-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "11j5jzqbzmwlws9zals43ry2f1nw9qy6im7yhn9ck5rikywrmm5z"))))
  (build-system perl-build-system)
  (arguments
   `(#:phases
     (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (args `("Makefile.PL"
                            ,(string-append "PREFIX=" out)
                            "INSTALLDIRS=site")))
               (setenv "CONFIG_SHELL" (which "sh"))
               (zero? (apply system* "perl" args))))))))
  (home-page "http://search.cpan.org/dist/Socket6")
  (synopsis
    "IPv6 related part of the C socket.h defines and structure manipulators for Perl")
  (description "Socket6 binds the IPv6 related part of the C socket header
definitions and structure manipulators for Perl.")
  (license license:bsd-3)))

(define-public perl-net-dns-resolver-programmable
 (package
  (name "perl-net-dns-resolver-programmable")
  (version "v0.003")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/J/JM/JMEHNLE/net-dns-resolver-programmable/"
             "Net-DNS-Resolver-Programmable-" version ".tar.gz"))
      (sha256
        (base32
          "1v3nl2kaj4fs55n1617n53q8sa3mir06898vpy1rq98zjih24h4d"))
      (patches
       (search-patches "perl-net-dns-resolver-programmable-fix.patch"))))
  (build-system perl-build-system)
  (native-inputs
    `(("perl-module-build" ,perl-module-build)))
  (inputs `(("perl-net-dns" ,perl-net-dns)))
  (home-page
    "http://search.cpan.org/dist/Net-DNS-Resolver-Programmable")
  (synopsis
    "Programmable DNS resolver class for offline emulation of DNS")
  (description "Net::DNS::Resolver::Programmable is a programmable DNS resolver for
offline emulation of DNS.")
  (license license:perl-license)))

(define-public perl-netaddr-ip
 (package
  (name "perl-netaddr-ip")
  (version "4.079")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/M/MI/MIKER/NetAddr-IP-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1rx0dinrz9fk9qcg4rwqq5n1dm3xv2arymixpclcv2q2nzgq4npc"))))
  (build-system perl-build-system)
  (arguments
    `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (args `("Makefile.PL"
                            ,(string-append "PREFIX=" out)
                            "INSTALLDIRS=site")))
               (setenv "CONFIG_SHELL" (which "sh"))
               (zero? (apply system* "perl" args))))))))
  (home-page
    "http://search.cpan.org/dist/NetAddr-IP")
  (synopsis
    "Manages IPv4 and IPv6 addresses and subnets")
  (description "NetAddr::IP manages IPv4 and IPv6 addresses and subsets.")
  (license license:perl-license)))

(define-public perl-net-patricia
 (package
  (name "perl-net-patricia")
  (version "1.22")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/G/GR/GRUBER/Net-Patricia-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0ln5f57vc8388kyh9vhx2infrdzfhbpgyby74h1qsnhwds95m0vh"))))
  (build-system perl-build-system)
  (inputs
    `(("perl-net-cidr-lite" ,perl-net-cidr-lite)
      ("perl-socket6" ,perl-socket6)))
  (home-page
    "http://search.cpan.org/dist/Net-Patricia")
  (synopsis
    "Patricia Trie Perl module for fast IP address lookups")
  (description
    "Net::Patricia does IP address lookups quickly in Perl.")
  ;; The bindings are licensed under GPL2 or later.
  ;; libpatricia is licensed under 2-clause BSD.
  (license (list license:gpl2+ license:bsd-2))))

(define-public perl-net-cidr-lite
 (package
  (name "perl-net-cidr-lite")
  (version "0.21")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/D/DO/DOUGW/Net-CIDR-Lite-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "14shj73zbqmfjbp0qz1fs9j4p2dpvz5hfkm4qfdjbydflbl2b8fg"))))
  (build-system perl-build-system)
  (home-page
    "http://search.cpan.org/dist/Net-CIDR-Lite")
  (synopsis
    "Perl extension for merging IPv4 or IPv6 CIDR addresses")
  (description "Net::CIDR::Lite merges IPv4 or IPv6 CIDR addresses.")
  (license license:gpl1+)))

;; TODO: Use the geolite-mirror-simple.pl script from the example
;; directory to stay current with the databases. How?
(define-public perl-geo-ip
 (package
  (name "perl-geo-ip")
  (version "1.51")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/M/MA/MAXMIND/Geo-IP-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1fka8fr7fw6sh3xa9glhs1zjg3s2gfkhi7n7da1l2m2wblqj0c0n"))))
  (build-system perl-build-system)
  (home-page "http://search.cpan.org/dist/Geo-IP")
  (synopsis
    "Look up location and network information by IP Address in Perl")
  (description "The Perl module 'Geo::IP'.  It looks up location and network
information by IP Address.")
  (license license:perl-license)))

(define-public perl-io-socket-inet6
 (package
  (name "perl-io-socket-inet6")
  (version "2.72")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/S/SH/SHLOMIF/IO-Socket-INET6-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1fqypz6qa5rw2d5y2zq7f49frwra0aln13nhq5gi514j2zx21q45"))))
  (build-system perl-build-system)
  (native-inputs
    `(("perl-module-build" ,perl-module-build)
      ("perl-test-pod" ,perl-test-pod)
      ("perl-test-pod-coverage" ,perl-test-pod-coverage)))
  (inputs `(("perl-socket6" ,perl-socket6)))
  (arguments `(;; Need network socket API
               #:tests? #f))
  (home-page
    "http://search.cpan.org/dist/IO-Socket-INET6")
  (synopsis
    "Perl object interface for AF_INET/AF_INET6 domain sockets")
  (description "IO::Socket::INET6 is an interface for AF_INET/AF_INET6 domain
sockets in Perl.")
  (license license:perl-license)))

(define-public libproxy
  (package
    (name "libproxy")
    (version "0.4.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libproxy/libproxy/"
                                  "releases/download/" version "/libproxy-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0kvdrazlzwia876w988cmlypp253gwy6idlh8mjk958c29jb8kb5"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("dbus" ,dbus)
       ("zlib" ,zlib)
       ("network-manager" ,network-manager)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
                  (lambda _
                    (zero? (system* "ctest" "-E" "url-test")))))))
    (synopsis "Library providing automatic proxy configuration management")
    (description "Libproxy handles the details of HTTP/HTTPS proxy
configuration for applications across all scenarios.  Applications using
libproxy only have to specify which proxy to use.")
    (home-page "https://libproxy.github.io/libproxy")
    (license license:lgpl2.1+)))

(define-public proxychains-ng
  (package
    (name "proxychains-ng")
    (version "4.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rofl0r/" name "/releases/"
                                  "download/v" version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0kiss3ih6cwayzvqi5cx4kw4vh7r2kfxlbgk56v1f1066ncm8aj8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-configure-script
           (lambda _
             ;; The configure script is very intolerant to unknown arguments,
             ;; such as "CONFIG_SHELL".
             (substitute* "configure"
               (("\\*\\) break ;;" line)
                (string-append "[A-Z]*) shift ;;\n"
                               line)))
             #t)))))
    (synopsis "Redirect any TCP connection through a proxy or proxy chain")
    (description "Proxychains-ng is a preloader which hooks calls to sockets
in dynamically linked programs and redirects them through one or more SOCKS or
HTTP proxies.")
    (home-page "https://github.com/rofl0r/proxychains-ng")
    (license license:gpl2+)))

(define-public enet
  (package
    (name "enet")
    (version "1.3.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://enet.bespin.org/download/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0p53mnmjbm56wizwraznynx13fcibcxiqny110dp6a5a3w174q73"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis
     "Network communication layer on top of UDP")
    (description
     "ENet's purpose is to provide a relatively thin, simple and robust network
communication layer on top of UDP.  The primary feature it provides is optional
reliable, in-order delivery of packets.  ENet omits certain higher level
networking features such as authentication, server discovery, encryption, or
other similar tasks that are particularly application specific so that the
library remains flexible, portable, and easily embeddable.")
    (home-page "http://enet.bespin.org")
    (license license:expat)))

(define-public sslh
  (package
    (name "sslh")
    (version "1.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/yrutschle/sslh/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vzw7a7s9lhspbn5zn3hw8hir4pkjgbd68yys4hfsnjp1h7bzjpn"))))
    (build-system gnu-build-system)
    (native-inputs
     `(;; Tests dependencies.
       ("lcov" ,lcov)
       ("perl" ,perl)
       ("perl-io-socket-inet6" ,perl-io-socket-inet6)
       ("perl-socket6" ,perl-socket6)
       ("psmisc" ,psmisc)
       ("valgrind" ,valgrind)))
    (inputs
     `(("libcap" ,libcap)
       ("libconfig" ,libconfig)
       ("tcp-wrappers" ,tcp-wrappers)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'check 'fix-tests
                     (lambda _
                       (substitute* "./t"
                         (("\"/tmp") "$ENV{\"TMPDIR\"} . \"")
                         ;; The Guix build environment lacks ‘ip6-localhost’.
                         (("ip6-localhost") "localhost"))
                       #t))
         ;; Many of these files are mentioned in the man page. Install them.
         (add-after 'install 'install-documentation
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (doc (string-append out "/share/doc/sslh")))
                        (install-file "README.md" doc)
                        (for-each
                         (lambda (file)
                           (install-file file (string-append doc "/examples")))
                         (append (find-files "." "\\.cfg")
                                 (find-files "scripts"))))
                      #t)))
       #:make-flags (list "CC=gcc"
                          "USELIBCAP=1"
                          "USELIBWRAP=1"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:test-target "test"))
    (home-page "http://www.rutschle.net/tech/sslh.shtml")
    (synopsis "Applicative network protocol demultiplexer")
    (description
     "sslh is a network protocol demultiplexer.  It acts like a switchboard,
accepting connections from clients on one port and forwarding them to different
servers based on the contents of the first received data packet.  Detection of
common protocols like HTTP(S), SSL, SSH, OpenVPN, tinc, and XMPP is already
implemented, but any other protocol that matches a regular expression can be
added.  sslh's name comes from its original application of serving both SSH and
HTTPS on port 443, allowing SSH connections from inside corporate firewalls
that block port 22.")
    (license (list license:bsd-2        ; tls.[ch]
                   license:gpl2+))))    ; everything else

(define-public iperf
  (package
    (name "iperf")
    (version "3.1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://downloads.es.net/pub/iperf"
                                  "/iperf-" version ".tar.gz"))
              (sha256
                (base32
                 "0kvk8d0a3dcxc8fisyprbn01y8akxj4sx8ld5dh508p9dx077vx4"))))
    (build-system gnu-build-system)
    (synopsis "TCP, UDP and SCTP bandwidth measurement tool")
    (description
     "iPerf is a tool to measure achievable bandwidth on IP networks.  It
supports tuning of various parameters related to timing, buffers and
protocols (TCP, UDP, SCTP with IPv4 and IPv6).  For each test it reports
the bandwidth, loss, and other parameters.")
    (home-page "http://software.es.net/iperf/")
    (license (list license:bsd-3             ; Main distribution.
                   license:ncsa              ; src/{units,iperf_locale,tcp_window_size}.c
                   license:expat             ; src/{cjson,net}.[ch]
                   license:public-domain)))) ; src/portable_endian.h

(define-public nethogs
  (package
    (name "nethogs")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/raboof/nethogs/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1k4x8r7s4dgcb6n2rjn28h2yyij92mwm69phncl3597cdxr954va"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs
     `(("libpcap" ,libpcap)
       ("ncurses" ,ncurses)))
    (arguments
     `(#:make-flags `("CC=gcc"
                      ,(string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; No ./configure script.
    (home-page "https://github.com/raboof/nethogs")
    (synopsis "Per-process bandwidth monitor")
    (description "NetHogs is a small 'net top' tool for Linux.  Instead of
breaking the traffic down per protocol or per subnet, like most tools do, it
groups bandwidth by process.

NetHogs does not rely on a special kernel module to be loaded.  If there's
suddenly a lot of network traffic, you can fire up NetHogs and immediately see
which PID is causing this.  This makes it easy to identify programs that have
gone wild and are suddenly taking up your bandwidth.")
    (license license:gpl2+)))

(define-public nzbget
  (package
    (name "nzbget")
    (version "19.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/nzbget/nzbget/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0y713g7gd4n5chbhr8lv7k50rxkmzysrg13sscxam3s386mmlb1r"))
       (modules '((guix build utils)))
       (snippet
        ;; Reported upstream as <https://github.com/nzbget/nzbget/pull/414>.
        '(begin
           (substitute* "daemon/connect/TlsSocket.cpp"
             (("gnutls_certificate-verification_status_print")
              "gnutls_certificate_verification_status_print"))
           #t))))
    (arguments
     `(#:configure-flags
       (list
        (string-append "--with-libcurses-includes="
                       (assoc-ref %build-inputs "ncurses") "/include")
        (string-append "--with-libcurses-libraries="
                       (assoc-ref %build-inputs "ncurses") "/lib")
        (string-append "--with-tlslib=GnuTLS"))))
    (build-system gnu-build-system)
    (inputs `(("gnutls", gnutls)
              ("libxml2", libxml2)
              ("ncurses", ncurses)
              ("zlib", zlib)))
    (native-inputs `(("pkg-config", pkg-config)))
    (home-page "https://github.com/nzbget/nzbget")
    (synopsis "Usenet binary file downloader")
    (description
     "NZBGet is a binary newsgrabber, which downloads files from Usenet based
on information given in @code{nzb} files.  NZBGet can be used in standalone
and in server/client modes.  In standalone mode, you pass NZBGet @command{nzb}
files as command-line parameters and it downloads them and exits.  NZBGet also
contains a Web interface.  Its server can be controlled through remote
procedure calls (RPCs).")
    (license license:gpl2+)))

(define-public openvswitch
  (package
    (name "openvswitch")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://openvswitch.org/releases/openvswitch-"
                    version ".tar.gz"))
              (sha256
               (base32
                "036gq741j9kqsjlp693nff838c9wjd1c56nswl9vyyd1lsmj0yrh"))))
    (build-system gnu-build-system)
    (arguments
     '(;; FIXME: many tests fail with:
       ;;    […]
       ;;    test -e $OVS_RUNDIR/ovs-vswitchd.pid
       ;;    ovs-appctl -t ovs-vswitchd exit
       ;;    hard failure
       #:tests? #f
       #:configure-flags
       '("--enable-shared"
         "--localstatedir=/var"
         "--with-dbdir=/var/lib/openvswitch")
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda _
             (zero? (system* "make"
                             ;; Don't try to create directories under /var.
                             "RUNDIR=/tmp"
                             "PKIDIR=/tmp"
                             "LOGDIR=/tmp"
                             "DBDIR=/tmp"
                             "install")))))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ;; for testing
       ("util-linux" ,util-linux)))
    (propagated-inputs
     `(("python-six" ,python2-six)))
    (inputs
     `(("libcap-ng" ,libcap-ng)
       ("openssl" ,openssl)))
    (synopsis "Virtual network switch")
    (home-page "http://www.openvswitch.org/")
    (description
     "Open vSwitch is a multilayer virtual switch.  It is designed to enable
massive network automation through programmatic extension, while still
supporting standard management interfaces and protocols (e.g. NetFlow, sFlow,
IPFIX, RSPAN, CLI, LACP, 802.1ag).")
    (license                            ; see debian/copyright for detail
     (list license:lgpl2.1              ; xenserver and utilities/bugtool
           license:gpl2                 ; datapath
           license:bsd-2 license:bsd-3
           license:asl2.0))))           ; all other

(define-public python-ipy
  (package
    (name "python-ipy")
    (version "0.83")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "IPy" version))
              (sha256
               (base32
                "1f6sdrxclifky4gvkf4gvyv5hx3fjh8vzspnfrqki6qm5d9mmnk1"))))
    (build-system python-build-system)
    (home-page "https://github.com/autocracy/python-ipy/")
    (synopsis "Python class and tools for handling IP addresses and networks")
    (description "The @code{IP} class allows a comfortable parsing and
handling for most notations in use for IPv4 and IPv6 addresses and
networks.")
    (license license:bsd-3)))

(define-public speedtest-cli
  (package
    (name "speedtest-cli")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/sivel/speedtest-cli/archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fbq4kpx8sj50g74hwpixisfjjgxq6zyn40d3m28dxhn7mxbnlrq"))))
    (build-system python-build-system)
    (home-page "https://github.com/sivel/speedtest-cli")
    (synopsis "Internet bandwidth tester")
    (description
     "Command line interface for testing internet bandwidth using
speedtest.net.")
    (license license:asl2.0)))

(define-public tftp-hpa
  (package
    (name "tftp-hpa")
    (version "5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/"
                                  "network/tftp/tftp-hpa/tftp-hpa-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "12vidchglhyc20znq5wdsbhi9mqg90jnl7qr9qs8hbvaz4fkdvmg"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; no test target
    (synopsis "HPA's tftp client")
    (description
     "This is a tftp client derived from OpenBSD tftp with some extra options
added and bugs fixed.  The source includes readline support but it is not
enabled due to license conflicts between the BSD advertising clause and the GPL.")
    (home-page "http://git.kernel.org/cgit/network/tftp/tftp-hpa.git/about/")
    ;; Some source files are distributed under a 3-clause BSD license, and
    ;; others under a 4-clause BSD license. Refer to the files in the source
    ;; distribution for clarification.
    (license (list license:bsd-3 license:bsd-4))))

(define-public pidentd
  (package
    (name "pidentd")
    (version "3.0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ptrrkssn/pidentd/archive/"
                           "v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0y3kd1bkydqkpc1qdff24yswysamsqivvadjy0468qri5730izgc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; No tests are included
    (inputs
     `(("openssl" ,openssl))) ; For the DES library
    (home-page "https://www.lysator.liu.se/~pen/pidentd/")
    (synopsis "Small Ident Daemon")
    (description
     "@dfn{Pidentd} (Peter's Ident Daemon) is a identd, which implements a
identification server.  Pidentd looks up specific TCP/IP connections and
returns the user name and other information about the connection.")
    (license license:public-domain)))

(define-public spiped
  (package
    (name "spiped")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.tarsnap.com/spiped/spiped-"
                                  version ".tgz"))
              (sha256
               (base32
                "1r51rdcl7nib1yv3yvgd5alwlkkwmr387brqavaklb0p2bwzixz6"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "test"
       #:make-flags (let* ((out (assoc-ref %outputs "out"))
                           (bindir (string-append out "/bin"))
                           (man1dir (string-append out "/share/man/man1")))
                      (list "CC=gcc" ; It tries to invoke `c99`.
                            (string-append "BINDIR=" bindir)
                            (string-append "MAN1DIR=" man1dir)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-command-invocations
           (lambda _
             (substitute* '("Makefile"
                            "libcperciva/cpusupport/Build/cpusupport.sh"
                            "libcperciva/POSIX/posix-cflags.sh"
                            "libcperciva/POSIX/posix-l.sh")
               (("command -p") ""))
             #t))
         (delete 'configure) ; No ./configure script.
         (add-after 'install 'install-more-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref %outputs "out"))
                    (misc (string-append out "/share/doc/spiped")))
               (install-file "DESIGN.md" misc)
               #t))))))
    (native-inputs
     `(("procps" ,procps))) ; `ps` is used by the test suite.
    (inputs
     `(("openssl" ,openssl)))
    (home-page "https://www.tarsnap.com/spiped.html")
    (synopsis "Create secure pipes between sockets")
    (description "Spiped (pronounced \"ess-pipe-dee\") is a utility for creating
symmetrically encrypted and authenticated pipes between socket addresses, so
that one may connect to one address (e.g., a UNIX socket on localhost) and
transparently have a connection established to another address (e.g., a UNIX
socket on a different system).  This is similar to 'ssh -L' functionality, but
does not use SSH and requires a pre-shared symmetric key.")
    (license license:bsd-2)))

(define-public quagga
  (package
    (name "quagga")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/quagga/quagga-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0c99rjjc62xl5kwvx2pwyvs0709vbwax1qydqbqf6r7fpvr24bjj"))
              (patches
               (search-patches "quagga-reproducible-build.patch"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config",pkg-config)
                     ("perl",perl)
                     ("dejagnu",dejagnu)))
    (inputs `(("readline",readline)
              ("c-ares",c-ares)))
    (synopsis "Routing Software Suite")
    (description "Quagga is a routing software suite, providing implementations
of OSPFv2, OSPFv3, RIP v1 and v2, RIPng and BGP-4 for Unix platforms.

The Quagga architecture consists of a core daemon, @command{zebra}, which
acts as an abstraction layer to the underlying Unix kernel and presents the
Zserv API over a Unix or TCP stream to Quagga clients.  It is these Zserv
clients which typically implement a routing protocol and communicate routing
updates to the zebra daemon.")
    (home-page "http://www.nongnu.org/quagga/")
    (license license:gpl2+)))

(define-public thc-ipv6
  (package
    (name "thc-ipv6")
    (version "3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/vanhauser-thc/thc-ipv6/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0yh2lpsazmm0pgbmh0dx023w6fss1kdfyr4cq7yw0fac8vkw32d3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f ; No test suite.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No ./configure script.
         (add-before 'build 'patch-paths
           (lambda _
             (substitute* "Makefile"
               (("/bin/echo") "echo"))
             #t))
         (add-after 'install 'install-more-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/thc-ipv6/doc")))
               (install-file "README" doc)
               (install-file "HOWTO-INJECT" doc)
               #t))))))
    ;; TODO Add libnetfilter-queue once packaged.
    (inputs
     `(("libpcap" ,libpcap)
       ("openssl" ,openssl)
       ("perl" ,perl)))
    (home-page "https://github.com/vanhauser-thc/thc-ipv6")
    (synopsis "IPv6 security research toolkit")
    (description "The THC IPv6 Toolkit provides command-line tools and a library
for researching IPv6 implementations and deployments.  It requires Linux 2.6 or
newer and only works on Ethernet network interfaces.")
    ;; AGPL 3 with exception for linking with OpenSSL. See the 'LICENSE' file in
    ;; the source distribution for more information.
    (license license:agpl3)))

(define-public bmon
  (package
    (name "bmon")
    (version "4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tgraf/bmon/releases/download/v"
                           version "/bmon-" version ".tar.gz"))
       (sha256
        (base32
         "0ylzriv4pwh76344abzl1w219x188gshbycbna35gsyfp09c7z82"))))
    (build-system gnu-build-system)
    (inputs
     `(("libconfuse" ,libconfuse)
       ("libnl" ,libnl)
       ("ncurses" ,ncurses)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Bandwidth monitor")
    (description "bmon is a monitoring and debugging tool to capture
networking-related statistics and prepare them visually in a human-friendly
way.  It features various output methods including an interactive curses user
interface and a programmable text output for scripting.")
    (home-page "https://github.com/tgraf/bmon")
    ;; README.md mentions both the 2-clause BSD and expat licenses, but all
    ;; the source files only have expat license headers. Upstream has been
    ;; contacted for clarification: https://github.com/tgraf/bmon/issues/59
    ;; Update the license field when upstream responds.
    (license (list license:bsd-2
                   license:expat))))

(define-public libnet
  (package
    (name "libnet")
    (version "1.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sam-github/libnet/"
                                  "archive/libnet-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0l4gbzzvr199fzczzricjz7b825i7dlk6sgl5p5alnkcagmq0xys"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "libnet") #t))
         (add-after 'chdir 'bootstrap
           (lambda _ (zero? (system* "autoreconf" "-vif"))))
         (add-before 'build 'build-doc
           (lambda* (#:key make-flags #:allow-other-keys)
             (zero? (apply system* "make" "-C" "doc" "doc"
                           make-flags)))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("doxygen" ,doxygen)))
    (home-page "https://sourceforge.net/projects/libnet-dev/")
    (synopsis "Framework for low-level network packet construction")
    (description
     "Libnet provides a fairly portable framework for network packet
construction and injection.  It features portable packet creation interfaces
at the IP layer and link layer, as well as a host of supplementary
functionality.  Using libnet, quick and simple packet assembly applications
can be whipped up with little effort.")
    (license license:bsd-2)))

(define-public mtr
  (package
    (name "mtr")
    (version "0.92")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.bitwizard.nl/" name "/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "10j3ds3p27jygys4x08kj8fi3zlsgiv72xsfazkah6plwawrv5zj"))))
    (build-system gnu-build-system)
    (inputs
     `(("libcap" ,libcap)
       ("ncurses" ,ncurses)))
    (native-inputs
     ;; The 0.92 release tarball still requires the ‘autoheader’ tool.
     `(("autoconf" ,autoconf)))
    (arguments
     `(#:tests? #f))                    ; tests require network access
    (home-page "https://www.bitwizard.nl/mtr/")
    (synopsis "Network diagnostic tool")
    (description
     "@dfn{mtr} (My TraceRoute) combines the functionality of the
@command{traceroute} and @command{ping} programs in a single network diagnostic
tool.  @command{mtr} can use several network protocols to detect intermediate
routers (or @dfn{hops}) between the local host and a user-specified destination.
It then continually measures the response time and packet loss at each hop, and
displays the results in real time.")
    (license license:gpl2+)))
