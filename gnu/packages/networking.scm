;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2016 Raimon Grau <raimonster@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016, 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016 Benz Schenk <benz.schenk@uzh.ch>
;;; Copyright © 2016, 2017 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Adam Van Ymeren <adam@vany.ca>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Tonton <tonton@riseup.net>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
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
  #:use-module (guix git-download)
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match))

;; The gnu.org ‘home’ for this GNU project is a directory listing with 1.6.0 as
;; the latest version.  The author's git repository, mentioned in the 1.6.0
;; README and otherwise legit-looking, contains a proper 1.7.0 release tarball
;; with many OUI updates.  Use it, even though it's also several years old now.
(define-public macchanger
  (package
    (name "macchanger")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/alobbs/macchanger/"
                           "releases/download/" version "/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "1gs5m0jxyprdp00w2qkbnaqm3ilkjz0q1gqdg4nzdm8g4xy73qns"))))
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
              (("^TESTS = .*") "TESTS = \n"))
            #t)))))
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
    (version "1.7.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.dest-unreach.org/socat/download/socat-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0lcj6zpra33xhgvhmz9l3cqz10v8ybafb8dd1yqkwf1rhy01ymp3"))))
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
             (invoke "make" "REAL_DAEMON_DIR=/etc" "linux")))
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
                bins))
             #t)))))
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

(define-public czmq
  (package
    (name "czmq")
    (version "4.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/zeromq/" name
                    "/releases/download/v" version
                    "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h5hrcsc30fcwb032vy5gxkq4j4vv1y4dj460rfs1hhxi0cz83zh"))))
    (build-system gnu-build-system)
    (arguments
     '(;; TODO Tests fail for some reason:
       ;;  * zauth: OK
       ;;  * zbeacon: OK (skipping test, no UDP broadcasting)
       ;; E: (czmq_selftest) 18-02-24 16:25:52 No broadcast interface found, (ZSYS_INTERFACE=lo)
       ;; make[2]: *** [Makefile:2245: check-local] Segmentation fault
       ;; make[2]: Leaving directory '/tmp/guix-build-czmq-4.1.0.drv-0/czmq-4.1.0'
       ;; make[1]: *** [Makefile:2032: check-am] Error 2
       ;; make[1]: Leaving directory '/tmp/guix-build-czmq-4.1.0.drv-0/czmq-4.1.0'
       ;; make: *** [Makefile:1588: check-recursive] Error 1
       ;; phase `check' failed after 19.4 seconds
       #:tests? #f
       #:configure-flags '("--enable-drafts")))
    (inputs
     `(("zeromq" ,zeromq)))
    (home-page "http://zeromq.org")
    (synopsis "High-level C bindings for ØMQ")
    (description
     "czmq provides bindings for the ØMQ core API that hides the differences
between different versions of ØMQ.")
    (license license:mpl2.0)))

(define-public cppzmq
  (let ((revision "0")
        (commit "d9f0f016c07046742738c65e1eb84722ae32d7d4"))
    (package
      (name "cppzmq")
      (version (string-append "4.2.2-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/zeromq/cppzmq")
                      (commit commit)))
                (sha256
                 (base32
                  "1gmqlm00y6xpa5m6d4ajq3ww63n2w7h4sy997wj81vcqmqx45b1f"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system cmake-build-system)
      (arguments '(#:tests? #f)) ; No tests.
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (inputs
       `(("zeromq" ,zeromq)))
      (home-page "http://zeromq.org")
      (synopsis "C++ bindings for the ØMQ messaging library")
      (description
       "This package provides header-only C++ bindings for ØMQ.  The header
files contain direct mappings of the abstractions provided by the ØMQ C API.")
      (license license:expat))))

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
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
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
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://libndp.org/files/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1dlinhl39va00v55qygjc9ap77yqf7xvn4rwmvdr49xhzzxhlj1c"))))
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
    (version "4.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/network/"
                                  "ethtool/ethtool-" version ".tar.xz"))
              (sha256
               (base32
                "1j6hyr809af2m3gqm11hdfwks5kljqy1ikspq3d9rhj29qv6r2mi"))))
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
               '(begin
                  (substitute* "Main.h"
                    (("#include <stdio.h>")
                     "#include <stdio.h>\n#include <stdlib.h>"))
                  #t))))
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
                                 (string-append bin "/ifstatus")))
                    #t)))))
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
                (string-append (assoc-ref inputs "net-tools") "/bin/")))
             #t))
         (add-before 'check 'delete-failing-tests
           ;; Avoid https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=802105.
           (lambda _
             (substitute* "tests/common.c"
               (("tcase_add_test\\(tc, \
test_parse_format_ipv(4(|_listen_all|_mapped_ipv6)|6)\\);")
                ""))
             #t)))
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
    (version "5.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://debian/pool/main/w/whois/"
                           name "_" version ".tar.xz"))
       (sha256
        (base32
         "0l7chmlvsl22r5cfm6fpm999z2n3sjrnx3ha8f8kf42cn4gmkriy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags (list "CC=gcc"
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'build 'setenv
           (lambda _
             (setenv "HAVE_ICONV" "1")
             #t)))))
    (inputs
     `(("libidn2" ,libidn2)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (synopsis "Intelligent client for the WHOIS directory service")
    (description
      "whois searches for an object in a @dfn{WHOIS} (RFC 3912) database.
It is commonly used to look up the registered users or assignees of an Internet
resource, such as a domain name, an IP address block, or an autonomous system.
It can automatically select the appropriate server for most queries.

For historical reasons, this package also includes @command{mkpasswd}, which
encrypts passwords using @code{crypt(3)} and is unrelated to the Expect command
of the same name.")
    (home-page "https://github.com/rfc1036/whois")
    (license license:gpl2+)))

(define-public wireshark
  (package
    (name "wireshark")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.wireshark.org/download/src/wireshark-"
                           version ".tar.xz"))
       (sha256
        (base32
         "17h0ixq7yr6scscjkidaj3dh5x6dfd3f97ggdxlklkz9nbsk0kxw"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-failing-test
           ;; Test 31/32 fails with errors like "Program reassemble_test is
           ;; not available".  Skipping it for now.
           (lambda _
             (substitute* "CMakeLists.txt"
               (("suite_unittests" all) (string-append "# " all)))
             #t)))
       ;; Build process chokes during `validate-runpath' phase.
       ;;
       ;; Errors are like the following:
       ;; "/gnu/store/...wireshark-3.0.0/lib/wireshark/plugins/3.0/epan/ethercat.so:
       ;; error: depends on 'libwireshark.so.12', which cannot be found in
       ;; RUNPATH".  That is, "/gnu/store/...wireshark-3.0.0./lib" doesn't
       ;; belong to RUNPATH.
       ;;
       ;; That’s not a problem in practice because "ethercat.so" is a plugin,
       ;; so it’s dlopen’d by a process that already provides "libwireshark".
       ;; For now, we disable this phase.
       #:validate-runpath? #f))
    (inputs
     `(("c-ares" ,c-ares)
       ("glib" ,glib)
       ("gnutls" ,gnutls)
       ("libcap" ,libcap)
       ("libgcrypt" ,libgcrypt)
       ("libnl" ,libnl)
       ("libpcap" ,libpcap)
       ("libssh" ,libssh)
       ("libxml2" ,libxml2)
       ("lz4" ,lz4)
       ("lua" ,lua-5.2)                 ;Lua 5.3 unsupported
       ("krb5" ,mit-krb5)
       ("qtbase" ,qtbase)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsvg" ,qtsvg)
       ("sbc" ,sbc)
       ("snappy" ,snappy)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("doxygen" ,doxygen)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("qttools" ,qttools)))
    (synopsis "Network traffic analyzer")
    (description "Wireshark is a network protocol analyzer, or @dfn{packet
sniffer}, that lets you capture and interactively browse the contents of
network frames.")
    (home-page "https://www.wireshark.org/")
    (license license:gpl2+)))

(define-public fping
  (package
    (name "fping")
    (version "4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://fping.org/dist/fping-"
                           version ".tar.gz"))
       (sha256
        (base32 "0jmnf4vmr43aiwk3h2b5qdsb95gxar8gz1yli8fswnm9nrs9ccvx"))))
    (build-system gnu-build-system)
    (home-page "https://fping.org/")
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

(define-public gandi.cli
  (package
    (name "gandi.cli")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32 "0vfzkw1avybjkf6fwqpf5m4kjz4c0qkkmj62f3jd0zx00vh5ly1d"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'embed-store-file-names
           (lambda _
             (substitute* (list "gandi/cli/modules/cert.py"
                                "gandi/cli/tests/commands/test_certificate.py")
               (("openssl") (which "openssl")))
             #t))
         (add-after 'install 'install-documentation
           ;; The included man page may be outdated but we install it anyway,
           ;; since it's mentioned in 'gandi --help' and better than nothing.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p man1)
               (with-output-to-file (string-append man1 "/gandi.1")
                 (lambda _
                   (invoke "rst2man.py" "gandicli.man.rst")))
               #t))))))
    (native-inputs
     `(("python-docutils" ,python-docutils)   ; for rst2man.py
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-tox" ,python-tox)))
    (inputs
     `(("openssl" ,openssl)
       ("python-click" ,python-click)
       ("python-ipy" ,python-ipy)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)))
    (home-page "https://cli.gandi.net")
    (synopsis "Command-line interface to the Gandi.net Web API")
    (description
     "This package provides a command-line client (@command{gandi}) to buy,
manage, and delete Internet resources from Gandi.net such as domain names,
virtual machines, and certificates.")
    (license license:gpl3+)))

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
                               "/bin/curl')")))
             #t)))))
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
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.aircrack-ng.org/aircrack-ng-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1jl30d0kibc82447fr3lgw75arik0l9729k94z76l7vl51y8mq4a"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("libgcrypt" ,libgcrypt)
       ("libnl" ,libnl)
       ("libpcap" ,libpcap)
       ("ethtool" ,ethtool)
       ("pcre" ,pcre)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags
       (list "--with-experimental=yes"  ; build wesside-ng, etc.
             "--with-gcrypt")           ; openssl's the default
       #:phases (modify-phases %standard-phases
                  (add-before 'bootstrap 'patch-evalrev
                    (lambda _
                      ;; Called by ./autogen.sh below, before the default
                      ;; ‘patch-shebangs’ phase has had a chance to run.
                      (substitute* "evalrev"
                        (("/bin/sh")
                         (which "sh")))
                      #t))
                  (replace 'bootstrap
                    (lambda _
                      ;; Patch shebangs in generated files before running
                      ;; ./configure.
                      (setenv "NOCONFIGURE" "please")
                      (invoke "bash" "./autogen.sh")))
                  (add-after 'build 'absolutize-tools
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((ethtool (string-append (assoc-ref inputs "ethtool")
                                                    "/sbin/ethtool")))
                        (substitute* "scripts/airmon-ng"
                          (("ethtool ")
                           (string-append ethtool " ")))
                        #t))))))
    (home-page "https://www.aircrack-ng.org")
    (synopsis "Assess WiFi network security")
    (description
     "Aircrack-ng is a complete suite of tools to assess WiFi network
security.  It focuses on different areas of WiFi security: monitoring,
attacking, testing, and cracking.  All tools are command-line driven, which
allows for heavy scripting.")
    (license (list license:gpl2+ license:bsd-3))))

(define-public perl-data-validate-ip
  (package
    (name "perl-data-validate-ip")
    (version "0.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/D/DR/DROLSKY/Data-Validate-IP-"
             version ".tar.gz"))
       (sha256
        (base32 "1mmppyzsh1w2z2h86kvzqxy56wxgs62a3kf8nvcnz76bblir5ap1"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-requires" ,perl-test-requires)))
    (propagated-inputs
     `(("perl-netaddr-ip" ,perl-netaddr-ip)))
    (home-page "https://metacpan.org/release/Data-Validate-IP")
    (synopsis "IPv4 and IPv6 validation methods")
    (description
     "This module provides several IP address validation subroutines that both
validate and untaint their input.  This includes both basic validation
(@code{is_ipv4()} and @code{is_ipv6()}) and special cases like checking whether
an address belongs to a specific network or whether an address is public or
private (reserved).")
    (license license:perl-license)))

(define-public perl-net-dns
 (package
  (name "perl-net-dns")
  (version "1.19")
  (source
    (origin
      (method url-fetch)
      (uri
       (list
        (string-append "https://www.net-dns.org/download/Net-DNS-"
                       version ".tar.gz")
        (string-append "mirror://cpan/authors/id/N/NL/NLNETLABS/Net-DNS-"
                       version ".tar.gz")))
      (sha256
       (base32 "1myc23vz0m42yyg8iw7bf1pdrmx9ql6fhl2vwk1vwf55v6yphqi0"))))
  (build-system perl-build-system)
  (inputs
    `(("perl-digest-hmac" ,perl-digest-hmac)))
  (home-page "https://www.net-dns.org/")
  (synopsis
    "Perl Interface to the Domain Name System")
  (description "Net::DNS is the Perl Interface to the Domain Name System.")
  (license license:x11)))

(define-public perl-socket6
 (package
  (name "perl-socket6")
  (version "0.29")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/U/UM/UMEMOTO/Socket6-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "054izici8klfxs8hr5rljib28plijpsfymy99xbzdp047bx1b2a6"))))
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
               (apply invoke "perl" args)))))))
  (home-page "https://metacpan.org/release/Socket6")
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
    "https://metacpan.org/release/Net-DNS-Resolver-Programmable")
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
               (apply invoke "perl" args)))))))
  (home-page
    "https://metacpan.org/release/NetAddr-IP")
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
    "https://metacpan.org/release/Net-Patricia")
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
    "https://metacpan.org/release/Net-CIDR-Lite")
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
  (home-page "https://metacpan.org/release/Geo-IP")
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
    "https://metacpan.org/release/IO-Socket-INET6")
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
                    (invoke "ctest" "-E" "url-test"))))))
    (synopsis "Library providing automatic proxy configuration management")
    (description "Libproxy handles the details of HTTP/HTTPS proxy
configuration for applications across all scenarios.  Applications using
libproxy only have to specify which proxy to use.")
    (home-page "https://libproxy.github.io/libproxy")
    (license license:lgpl2.1+)))

(define-public proxychains-ng
  (package
    (name "proxychains-ng")
    (version "4.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://ftp.barfooze.de/pub/sabotage/tarballs/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0418fv8hgf43rzrxxlybg49jz2h6w8inndhb6v1184k4cwzjnl3p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
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
             #t))
         (add-before 'configure 'set-up-environment
           (lambda _
             (setenv "CC" "gcc")
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
    (version "1.3.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://enet.bespin.org/download/"
                                  "enet-" version ".tar.gz"))
              (sha256
               (base32
                "0w780zc6cy8yq4cskpphx0f91lzh51vh9lwyc5ll8hhamdxgbxlq"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Network communication layer on top of UDP")
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
    (version "1.19c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/yrutschle/sslh.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14x3n25n9md0sw8cda9m5bd8r96xpw1vdkapklw4mxgfcz1k2kxq"))))
    (build-system gnu-build-system)
    (native-inputs
     `(;; Test dependencies.
       ("lcov" ,lcov)
       ("perl" ,perl)
       ("perl-io-socket-inet6" ,perl-io-socket-inet6)
       ("perl-socket6" ,perl-socket6)
       ("psmisc" ,psmisc)
       ("valgrind" ,valgrind)))
    (inputs
     `(("libcap" ,libcap)
       ("libconfig" ,libconfig)
       ("pcre" ,pcre)
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
    (home-page "https://www.rutschle.net/tech/sslh/README.html")
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
    (version "20.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/nzbget/nzbget/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0q93aqqyzccn5r9sny38499rmg846qdh9pi2v5kvf9m23v54yk60"))
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
    (inputs `(("gnutls" ,gnutls)
              ("libxml2" ,libxml2)
              ("ncurses" ,ncurses)
              ("zlib" ,zlib)))
    (native-inputs `(("pkg-config" ,pkg-config)))
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
    (version "2.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://openvswitch.org/releases/openvswitch-"
                    version ".tar.gz"))
              (sha256
               (base32
                "103wi6k8xal0z1pyy680gms56skagh6jawa2ja588ljr55jcg4sg"))))
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
             (invoke "make"
                     ;; Don't try to create directories under /var.
                     "RUNDIR=/tmp"
                     "PKIDIR=/tmp"
                     "LOGDIR=/tmp"
                     "DBDIR=/tmp"
                     "install"))))))
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

(define-public python2-ipy
  (package-with-python2 python-ipy))

(define-public speedtest-cli
  (package
    (name "speedtest-cli")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sivel/speedtest-cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vv2z37g2kgm2dzkfa4bhri92hs0d1acxi8z66gznsl5148q7sdi"))))
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
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/quagga/quagga-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1lsksqxij5f1llqn86pkygrf5672kvrqn1kvxghi169hqf1c0r73"))
              (patches
               (search-patches "quagga-reproducible-build.patch"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("perl" ,perl)
                     ("dejagnu" ,dejagnu)))
    (inputs `(("readline" ,readline)
              ("c-ares" ,c-ares)))
    (synopsis "Routing Software Suite")
    (description "Quagga is a routing software suite, providing implementations
of OSPFv2, OSPFv3, RIP v1 and v2, RIPng and BGP-4 for Unix platforms.

The Quagga architecture consists of a core daemon, @command{zebra}, which
acts as an abstraction layer to the underlying Unix kernel and presents the
Zserv API over a Unix or TCP stream to Quagga clients.  It is these Zserv
clients which typically implement a routing protocol and communicate routing
updates to the zebra daemon.")
    (home-page "https://www.nongnu.org/quagga/")
    (license license:gpl2+)))

(define-public thc-ipv6
  (let ((revision "0")
        (commit "4bb72573e0950ce6f8ca2800a10748477020029e"))
    (package
      (name "thc-ipv6")
      (version (git-version "3.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/vanhauser-thc/thc-ipv6.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1x5i6vbsddqc2yks7r1a2fw2fk16qxvd6hpzh1lykjfpkal8fdir"))))
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
      (license license:agpl3))))

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
    (version "1.2-rc3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sam-github/libnet")
             (commit (string-append "libnet-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cy8w4g5rv963v4p6iq3333kxgdddx2lywp70xf62553a25xhhs4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "libnet") #t))
         (add-before 'build 'build-doc
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "-C" "doc" "doc"
                    make-flags))))))
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

(define-public strongswan
  (package
    (name "strongswan")
    (version "5.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.strongswan.org/strongswan-"
                           version ".tar.bz2"))
       (sha256
        (base32 "095zg7h7qwsc456sqgwb1lhhk29ac3mk5z9gm6xja1pl061driy3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-command-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/libstrongswan/utils/process.c"
               (("/bin/sh")
                (string-append (assoc-ref inputs "bash") "/bin/sh")))

             (substitute* "src/libstrongswan/tests/suites/test_process.c"
               (("/bin/sh") (which "sh"))
               (("/bin/echo") (which "echo"))
               (("cat") (which "cat")))
             #t))
         (add-before 'check 'set-up-test-environment
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZDIR" (string-append (assoc-ref inputs "tzdata")
                                            "/share/zoneinfo"))
             #t)))
       #:configure-flags
       (list
        ;; Disable bsd-4 licensed plugins
        "--disable-des"
        "--disable-blowfish")))
    (inputs
     `(("curl" ,curl)
       ("gmp" ,gmp)
       ("libgcrypt" ,libgcrypt)
       ("openssl" ,openssl)))
    (native-inputs
     `(("coreutils" ,coreutils)
       ("tzdata" ,tzdata-for-tests)))
    (synopsis "IKEv1/v2 keying daemon")
    (description "StrongSwan is an IPsec implementation originally based upon
the FreeS/WAN project.  It contains support for IKEv1, IKEv2, MOBIKE, IPv6,
NAT-T and more.")
    (home-page "https://strongswan.org/")
    (license
     (list license:gpl2+
           ;; src/aikgen/*
           ;; src/libcharon/plugins/dnscert/*
           ;; src/libcharon/plugins/ext_auth/*
           ;; src/libcharon/plugins/vici/ruby/*
           ;; src/libcharon/plugins/xauth_pam/xauth_pam_listener.[ch]
           license:expat
           ;; src/inclue/sys/*
           license:bsd-3
           ;; src/libstrongswan/plugins/sha3/sha3_keccak.c
           license:public-domain
           ;; src/libstrongswan/plugins/pkcs11/pkcs11.h
           (license:non-copyleft
            "file://src/libstrongswan/plugins/pkcs11/pkcs11.h"
            "pkcs11 contains a unknown permissive license. View the specific
file for more details.")
           ;; These files are not included in the
           ;; build, they are disabled through
           ;; options to ./configure
           ;;
           ;; src/libstrongswan/plugins/blowfish/bf_enc.c
           ;; src/libstrongswan/plugins/blowfish/bf_locl.h
           ;; src/libstrongswan/plugins/blowfish/bf_pi.h
           ;; src/libstrongswan/plugins/blowfish/bf_skey.c
           ;; src/libstrongswan/plugins/blowfish/blowfish_crypter.c
           ;; src/libstrongswan/plugins/des/des_crypter.c
           license:bsd-4))))

(define-public amule
  (package
    (name "amule")
    (version "2.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/amule-project/amule")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "010wxm6g9f92x6fympj501zbnjka32rzbx0sk3a2y4zpih5d2nsn"))
              ;; Patch for adopting crypto++ >= 6.0.
              (patches (search-patches "amule-crypto-6.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'bootstrap) ; bootstrap phase runs too early.
         (add-after 'patch-source-shebangs 'autogen
           (lambda _
             (invoke "sh" "autogen.sh")
             #t)))
       #:configure-flags
       '("--disable-rpath"
         "--enable-wxcas"
         "--enable-cas"
         "--enable-alc"
         "--enable-alcc"
         "--enable-xas"
         "--enable-amulecmd"
         "--enable-geoip"
         "--enable-ccache"
         "--enable-nls"
         "--enable-optimize"
         "--enable-amule-gui"
         "--enable-amule-daemon"
         "--enable-webserver"
         "--with-denoise-level=0")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext-minimal" ,gettext-minimal)
       ("perl" ,perl)))
    (inputs
     `(("zlib" ,zlib)
       ("crypto++" ,crypto++)
       ("libpng" ,libpng)
       ("wxwidgets-gtk2" ,wxwidgets-gtk2)))
    (home-page "http://amule.org/")
    (synopsis "Peer-to-peer client for the eD2K and Kademlia networks")
    (description
     "aMule is an eMule-like client for the eD2k and Kademlia peer-to-peer
file sharing networks.  It includes a graphical user interface (GUI), a daemon
allowing you to run a client with no graphical interface, and a Web GUI for
remote access.  The @command{amulecmd} command allows you to control aMule
remotely.")
    (license license:gpl2+)))

(define-public zyre
  (package
    (name "zyre")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/zeromq/zyre/releases/download/v"
                              version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qz2730bng1gs9xbqxhkw88qbsmszgmmrl2g9k6xrg6r3bqvsdc7"))))
    (build-system gnu-build-system)
    (inputs `(("zeromq" ,zeromq)
              ("czmq" ,czmq)
              ("libsodium" ,libsodium)))
    (synopsis "Framework for proximity-based peer-to-peer applications")
    (description "Zyre provides reliable group messaging over local area
networks using zeromq.  It has these key characteristics:

@itemize
@item Zyre needs no administration or configuration.
@item Peers may join and leave the network at any time.
@item Peers talk to each other without any central brokers or servers.
@item Peers can talk directly to each other.
@item Peers can join groups, and then talk to groups.
@item Zyre is reliable, and loses no messages even when the network is heavily loaded.
@item Zyre is fast and has low latency, requiring no consensus protocols.
@item Zyre is designed for WiFi networks, yet also works well on Ethernet networks.
@end itemize")
    (home-page "https://github.com/zeromq/zyre")
    (license license:mpl2.0)))

(define-public can-utils
  (package
    (name "can-utils")
    (version "2018.02.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linux-can/can-utils.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r0zkm67bdcmbfypjr7z041d4zp0xzb379dyl8cvhmflh12fd2jb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests exist.
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure))))
    (home-page "https://github.com/linux-can/can-utils")
    (synopsis "CAN utilities")
    (description "This package provides CAN utilities in the following areas:

@itemize
@item Basic tools to display, record, generate and replay CAN traffic
@item CAN access via IP sockets
@item CAN in-kernel gateway configuration
@item CAN bus measurement and testing
@item ISO-TP (ISO15765-2:2016 - this means messages with a body larger than
eight bytes) tools
@item Log file converters
@item Serial Line Discipline configuration for slcan driver
@end itemize")
    ;; Either BSD-3 or GPL-2 can be used.
    (license (list license:bsd-3 license:gpl2))))

(define-public asio
  (package
    (name "asio")
    (version "1.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/asio/asio/"
                           version " (Stable)/asio-" version ".tar.bz2"))
       (sha256
        (base32 "1akray4l3hgahmb92sbvsqg128c7g7s92jrkf1sp1fjnfjrxq9sf"))))
    (build-system gnu-build-system)
    (inputs
     `(("boost" ,boost)
       ("openssl" ,openssl)))
    (arguments
     `(#:configure-flags
       (list
        (string-append "--with-boost=" (assoc-ref %build-inputs "boost"))
        (string-append "--with-openssl=" (assoc-ref %build-inputs "openssl")))))
    (home-page "https://think-async.com/Asio")
    (synopsis "C++ library for ASynchronous network I/O")
    (description "Asio is a cross-platform C++ library for network and
low-level I/O programming that provides developers with a consistent
asynchronous model using a modern C++ approach.")
    (license license:boost1.0)))

(define-public shadowsocks
  ;; There are some security fixes after the last release.
  (let* ((commit "e332ec93e9c90f1cbee676b022bf2c5d5b7b1239")
         (revision "0")
         (version (git-version "2.8.2" revision commit)))
    (package
      (name "shadowsocks")
      (version version)
      (home-page "https://github.com/shadowsocks/shadowsocks")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "1idd9b4f2pnhcpk1bh030hqg5zq25gkwxd53xi3c0cj242w7sp2j"))
                (file-name (git-file-name name version))))
      (build-system python-build-system)
      (synopsis "Fast tunnel proxy that helps you bypass firewalls")
      (description
       "This package is a fast tunnel proxy that helps you bypass firewalls.

Features:
@itemize
@item TCP & UDP support
@item User management API
@item TCP Fast Open
@item Workers and graceful restart
@item Destination IP blacklist
@end itemize")
      (license license:asl2.0))))

(define-public net-snmp
  (package
    (name "net-snmp")
    (version "5.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/net-snmp/net-snmp/"
                                  version "/net-snmp-" version ".tar.gz"))
              (sha256
               (base32
                "1pvajzj9gmj56dmwix0ywmkmy2pglh6nny646hkm7ghfhh03bz5j"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Drop bundled libraries.
                  (delete-file-recursively "snmplib/openssl")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       ;; XXX: With parallel build enabled, Perl modules may not get linked with
       ;; libnetsnmp.  See e.g. <https://bugzilla.novell.com/show_bug.cgi?id=819497>.
       #:parallel-build? #f
       #:configure-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib")
             "--with-logfile=/var/log/snmpd.log"
             (string-append "--with-openssl="
                            (assoc-ref %build-inputs "openssl")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "testing/fulltests/support/simple_TESTCONF.sh"
               (("NETSTAT=\"\"")
                (string-append "NETSTAT=\"" (which "netstat") "\"")))
             (substitute* '("testing/fulltests/default/T065agentextend_simple"
                            "testing/fulltests/default/T115agentxperl_simple")
               (("/usr/bin/env") (which "env")))
             (substitute* "testing/fulltests/default/T065agentextend_sh_simple"
               (("/bin/sh") (which "sh")))
             ;; These tests require network access.
             (for-each delete-file
                       '("testing/fulltests/default/T070com2sec_simple"
                         "testing/fulltests/default/T071com2sec6_simple"))
             #t))
         (add-after 'unpack 'patch-Makefile.PL
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("Makefile.PL -NET")
                (string-append "Makefile.PL PREFIX="
                               (assoc-ref outputs "out")
                               " INSTALLDIRS=site" " NO_PERLLOCAL=1"
                               " -NET")))
             #t)))))
    (inputs
     `(("perl" ,perl)
       ("openssl" ,openssl)
       ("libnl" ,libnl)))
    ;; These inputs are only needed for tests.
    (native-inputs
     `(("net-tools" ,net-tools)
       ("coreutils" ,coreutils)
       ("grep" ,grep)))
    (home-page "http://www.net-snmp.org/")
    (synopsis "Simple Network Management Protocol library and tools")
    (description "The @dfn{Simple Network Management Protocol} (SNMP) is a
widely used protocol for monitoring the health and welfare of network
equipment (e.g. routers), computer equipment and even devices like UPSs.
Net-SNMP is a suite of applications used to implement SNMP v1, SNMP v2c and
SNMP v3 using both IPv4 and IPv6.")
    (license (list license:bsd-3
                   (license:non-copyleft
                    "http://www.net-snmp.org/about/license.html"
                    "CMU/UCD copyright notice")))))

(define-public ubridge
  (package
    (name "ubridge")
    (version "0.9.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/GNS3/ubridge/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hivb8wqkk5047bdl2vbsbcvkmam1107hx1ahy4virq2bkqki1fj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'set-bindir
           (lambda* (#:key  inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (mkdir-p bin)
               (substitute* "Makefile"
                 (("\\$\\(BINDIR\\)") bin)
                 (("\tsetcap cap_net.*$") "")))
             #t)))))
    (inputs
     `(("libpcap" ,libpcap)))
    (home-page "https://github.com/GNS3/ubridge/")
    (synopsis "Bridge for UDP tunnels, Ethernet, TAP and VMnet interfaces")
    (description "uBridge is a simple program to create user-land bridges
between various technologies.  Currently, bridging between UDP tunnels,
Ethernet and TAP interfaces is supported.  Packet capture is also supported.")
    (license license:gpl3+)))

(define-public hcxtools
  (let* ((commit "2ecfc9a06c2028c47522ea566ccd82b2c1f94647"))
    (package
      (name "hcxtools")
      (version (git-version "0.0.0" "1" commit))
      (home-page "https://github.com/ZerBea/hcxtools")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "1hzwrpmxjxl674if0pp5iq06mdi24k7ni7bh1h20isp4s40201n3"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (inputs
       `(("curl" ,curl)
         ("libpcap" ,libpcap)
         ("openssl" ,openssl)
         ("zlib" ,zlib)))
      (arguments
       `(#:make-flags (list "CC=gcc"
                            (string-append "INSTALLDIR=" (assoc-ref %outputs "out") "/bin"))
         #:tests? #f                    ;no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'set-environment
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "C_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "curl")
                                    "/include:"
                                    (assoc-ref inputs "libpcap")
                                    "/include:"
                                    (assoc-ref inputs "openssl")
                                    "/include:"
                                    (assoc-ref inputs "zlib")
                                    "/include:"
                                    (getenv "C_INCLUDE_PATH")))
             #t)))))
      (synopsis "Capture wlan traffic to hashcat and John the Ripper")
      (description
       "This package contains a small set of tools to capture and convert
packets from wireless devices for use with hashcat or John the Ripper.")
      (license license:expat))))

(define-public hcxdumptool
  (let* ((commit "f4799b5da82c5b030a6d99b02d1c1b9dc838ad36"))
    (package
      (name "hcxdumptool")
      (version (git-version "0.0.0" "1" commit))
      (home-page "https://github.com/ZerBea/hcxdumptool")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "0qlsin0rws9sshn12faq4spmd0ffzssal36s71vhv6gkhhga7abl"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "CC=gcc"
                            (string-append "INSTALLDIR=" (assoc-ref %outputs "out") "/bin"))
         #:tests? #f                    ;no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))
      (synopsis "Small tool to capture packets from wlan devices")
      (description
       "Small tool to capture packets from WLAN devices.  After capturing,
upload the \"uncleaned\" cap to @url{https://wpa-sec.stanev.org/?submit} to
see if the access point or the client is vulnerable to a dictionary attack.
Convert the cap file to hccapx format and/or to WPA-PMKID-PBKDF2
hashline (16800) with @command{hcxpcaptool} from the @code{hcxtools} package
and check if the WLAN key or the master key was transmitted unencrypted.")
      (license license:expat))))

(define-public dante
  (package
    (name "dante")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.inet.no/dante/files/dante-"
                           version ".tar.gz"))
       (sha256
        (base32
         "19rqzj167q73ag20zxpvswhkk0bj56r5maf83v5016sw7vrcz5sc"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: The dynamic socks library doesn't work with 'libc.so' (GNU ld
     ;; script).  When preloading is enabled, 'sockd' failed with:
     ;;    … Failed to open library "libc.so": …: invalid ELF header
     '(#:configure-flags '("--disable-preload")))
    (home-page "https://www.inet.no/dante/")
    (synopsis "SOCKS server and client")
    (description "Dante is a SOCKS client and server implementation.  It can
be installed on a machine with access to an external TCP/IP network and will
allow all other machines, without direct access to that network, to be relayed
through the machine the Dante server is running on.  The external network will
never see any machines other than the one Dante is running on.")
    (license (license:non-copyleft "file://LICENSE"))))

(define-public restbed
  (let ((commit "6eb385fa9051203f28bf96cc1844bbb5a9a6481f"))
    (package
      (name "restbed")
      (version (git-version "4.6" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Corvusoft/restbed/")
               (commit commit)))
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32 "0k60i5drklqqrb4khb25fzkgz9y0sncxf1sp6lh2bm1m0gh0661n"))))
      (build-system cmake-build-system)
      (inputs
       `(("asio" ,asio)
         ("catch" ,catch-framework)
         ("openssl" ,openssl)))
      (arguments
       `(#:tests? #f
         #:configure-flags
         '("-DBUILD_TESTS=NO"
           "-DBUILD_EXAMPLES=NO"
           "-DBUILD_SSL=NO"
           "-DBUILD_SHARED=NO")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'apply-patches-and-fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((asio (assoc-ref inputs "asio"))
                     (catch (assoc-ref inputs "catch"))
                     (openssl (assoc-ref inputs "openssl")))
                 (substitute* "cmake/Findasio.cmake"
                   (("(find_path\\( asio_INCLUDE asio\\.hpp HINTS ).*$" all begin)
                    (string-append begin " \"" asio "/include\" )")))
                 (substitute* "cmake/Findcatch.cmake"
                   (("(find_path\\( catch_INCLUDE catch\\.hpp HINTS ).*$" all begin)
                    (string-append begin " \"" catch "/include\" )")))
                 (substitute* "cmake/Findopenssl.cmake"
                   (("(find_library\\( ssl_LIBRARY ssl ssleay32 HINTS ).*$" all begin)
                    (string-append begin " \"" openssl "/lib\" )"))
                   (("(find_library\\( crypto_LIBRARY crypto libeay32 HINTS ).*$" all begin)
                    (string-append begin " \"" openssl "/lib\" )"))
                   (("(find_path\\( ssl_INCLUDE openssl/ssl\\.h HINTS ).*$" all begin)
                    (string-append begin " \"" openssl "/include\" )")))))))))
      (synopsis "Asynchronous RESTful functionality to C++11 applications")
      (description "Restbed is a comprehensive and consistent programming
model for building applications that require seamless and secure
communication over HTTP.")
      (home-page "https://github.com/Corvusoft/restbed")
      (license license:agpl3+))))

(define-public opendht
  (package
    (name "opendht")
    (version "1.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/savoirfairelinux/opendht.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vninb5mak27wigajslyvr05vq7wbrwqhbr4wzl2nmqcb20wmlq2"))))
    (build-system gnu-build-system)
    (inputs
     `(("gnutls" ,gnutls)
       ("nettle" ,nettle)
       ("readline" ,readline)
       ("jsoncpp" ,jsoncpp)
       ("restbed" ,restbed)))
    (propagated-inputs
     `(("argon2" ,argon2)               ; TODO: Needed for the pkg-config .pc file to work?
       ("msgpack" ,msgpack)))           ;included in several installed headers
    (native-inputs
     `(("autoconf" ,autoconf)
       ("pkg-config" ,pkg-config)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     `(#:configure-flags '("--disable-tools"
                           "--disable-python"
                           "--with-argon2")))
    (home-page "https://github.com/savoirfairelinux/opendht/")
    (synopsis "Distributed Hash Table (DHT) library")
    (description "OpenDHT is a Distributed Hash Table (DHT) library.  It may
be used to manage peer-to-peer network connections as needed for real time
communication.")
    (license license:gpl3+)))

(define-public frrouting
  (package
    (name "frrouting")
    (version "6.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FRRouting/frr/releases/"
                                  "download/frr-" version "/frr-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0xfrvi62w8qlh46f504ka0skb7pm0g0p8vmdng4w90gsbirlzpdd"))))
    (build-system gnu-build-system)
    (inputs
     `(("c-ares" ,c-ares)
       ("json-c" ,json-c)
       ("readline" ,readline)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("python-pytest" ,python-pytest)))
    (home-page "https://frrouting.org/")
    (synopsis "IP routing protocol suite")
    (description "FRRouting (FRR) is an IP routing protocol suite which includes
protocol daemons for BGP, IS-IS, LDP, OSPF, PIM, and RIP. ")
    (license license:gpl2+)))
