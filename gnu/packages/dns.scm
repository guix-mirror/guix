;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016, 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu packages dns)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial))

(define-public dnsmasq
  (package
    (name "dnsmasq")
    (version "2.80")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.thekelleys.org.uk/dnsmasq/dnsmasq-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1fv3g8vikj3sn37x1j6qsywn09w1jipvlv34j3q5qrljbrwa5ayd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("dbus" ,dbus)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases (delete 'configure))
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc"
                          "COPTS=\"-DHAVE_DBUS\"")
       #:tests? #f))                    ; no ‘check’ target
    (home-page "http://www.thekelleys.org.uk/dnsmasq/doc.html")
    (synopsis "Small caching DNS proxy and DHCP/TFTP server")
    (description
     "Dnsmasq is a light-weight DNS forwarder and DHCP server.  It is designed
to provide DNS and, optionally, DHCP to a small network.  It can serve the
names of local machines which are not in the global DNS.  The DHCP server
integrates with the DNS server and allows machines with DHCP-allocated
addresses to appear in the DNS with names configured either on each host or in
a central configuration file.  Dnsmasq supports static and dynamic DHCP leases
and BOOTP/TFTP for network booting of diskless machines.")
    ;; Source files only say GPL2 and GPL3 are allowed.
    (license (list license:gpl2 license:gpl3))))

;; 'bind' is the name of a built-in Guile procedure, which is why we choose a
;; different name here.
(define-public isc-bind
  (package
    (name "bind")
    (version "9.12.3-P1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://ftp.isc.org/isc/bind9/" version "/" name "-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0wzdbn6ig851354cjdys5q3gvqcvl2gmmih1gzr8ldl7sy4r7dvc"))))
    (build-system gnu-build-system)
    (outputs `("out" "utils"))
    (inputs
     ;; it would be nice to add GeoIP and gssapi once there is package
     `(("libcap" ,libcap)
       ("libxml2" ,libxml2)
       ("openssl" ,openssl)
       ("p11-kit" ,p11-kit)))
    (native-inputs `(("perl" ,perl)
                     ("net-tools" ,net-tools)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-openssl="
                            (assoc-ref %build-inputs "openssl"))
             (string-append "--with-pkcs11="
                            (assoc-ref %build-inputs "p11-kit")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'strip 'move-to-utils
           (lambda _
             (for-each
              (lambda (file)
                (let ((target  (string-append (assoc-ref %outputs "utils") file))
                      (src  (string-append (assoc-ref %outputs "out") file)))
                  (mkdir-p (dirname target))
                  (link src target)
                  (delete-file src)))
              '("/bin/dig" "/bin/delv" "/bin/nslookup" "/bin/host" "/bin/nsupdate"
                "/share/man/man1/dig.1"
                "/share/man/man1/host.1"
                "/share/man/man1/nslookup.1"
                "/share/man/man1/nsupdate.1"))))
         ;; When and if guix provides user namespaces for the build process,
         ;; then the following can be uncommented and the subsequent "force-test"
         ;; will not be necessary.
         ;;
         ;;   (add-before 'check 'set-up-loopback
         ;;     (lambda _
         ;;          (system "bin/tests/system/ifconfig.sh up")))
         (replace 'check
           (lambda _
             (invoke "make" "force-test")
             #t)))))
    (synopsis "An implementation of the Domain Name System")
    (description "BIND is an implementation of the @dfn{Domain Name System}
(DNS) protocols for the Internet.  It is a reference implementation of those
protocols, but it is also production-grade software, suitable for use in
high-volume and high-reliability applications. The name BIND stands for
\"Berkeley Internet Name Domain\", because the software originated in the early
1980s at the University of California at Berkeley.")
    (home-page "https://www.isc.org/downloads/bind")
    (license (list license:mpl2.0))))

(define-public dnscrypt-proxy
  (package
    (name "dnscrypt-proxy")
    (version "1.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.dnscrypt.org/dnscrypt-proxy/"
                    "dnscrypt-proxy-" version ".tar.bz2"))
              (sha256
               (base32
                "1dhvklr4dg2vlw108n11xbamacaryyg3dbrg629b76lp7685p7z8"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete bundled libltdl. XXX: This package also bundles
               ;; a modified libevent that cannot currently be removed.
               '(begin
                  (delete-file-recursively "libltdl")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoreconf
           (lambda _
             ;; Re-generate build files due to unbundling ltdl.
             ;; TODO: Prevent generating new libltdl and building it.
             ;; The system version is still favored and referenced.
             (invoke "autoreconf" "-vif"))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("automake" ,automake)
       ("autoconf" ,autoconf)
       ("libtool" ,libtool)))
    (inputs
     `(("libltdl" ,libltdl)
       ("libsodium" ,libsodium)))
    (home-page "https://www.dnscrypt.org/")
    (synopsis "Securely send DNS requests to a remote server")
    (description
     "@command{dnscrypt-proxy} is a tool for securing communications
between a client and a DNS resolver.  It verifies that responses you get
from a DNS provider was actually sent by that provider, and haven't been
tampered with.  For optimal performance it is recommended to use this as
a forwarder for a caching DNS resolver such as @command{dnsmasq}, but it
can also be used as a normal DNS \"server\".  A list of public dnscrypt
servers is included, and an up-to-date version is available at
@url{https://download.dnscrypt.org/dnscrypt-proxy/dnscrypt-resolvers.csv}.")
    (license (list license:isc
                   ;; Libevent and src/ext/queue.h is 3-clause BSD.
                   license:bsd-3))))

(define-public dnscrypt-wrapper
  (package
    (name "dnscrypt-wrapper")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cofyc/dnscrypt-wrapper/releases"
                    "/download/v" version "/" name "-v" version ".tar.bz2"))
              (sha256
               (base32
                "1vhg4g0r687f51wcdn7z9w1hxapazx6vyh5rsr8wa48sljzd583g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("CC=gcc")
       ;; TODO: Tests require ruby-cucumber and ruby-aruba.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-configure
           (lambda _
             (invoke "make" "configure"))))))
    (native-inputs
     `(("autoconf" ,autoconf)))
    (inputs
     `(("libevent" ,libevent)
       ("libsodium" ,libsodium)))
    (home-page "https://github.com/Cofyc/dnscrypt-wrapper")
    (synopsis "Server-side dnscrypt proxy")
    (description
     "@command{dnscrypt-wrapper} is a tool to expose a name server over
the @code{dnscrypt} protocol.  It can be used as an endpoint for the
@command{dnscrypt-proxy} client to securely tunnel DNS requests between
the two.")
    (license (list license:isc
                   ;; Bundled argparse is MIT. TODO: package and unbundle.
                   license:expat
                   ;; dns-protocol.h and rfc1035.{c,h} is gpl2 or gpl3 (either).
                   license:gpl2
                   license:gpl3))))

(define-public libasr
  (package
    (name "libasr")
    (version "201602131606")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.opensmtpd.org/archives/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "18kdmbjsxrfai16d66qslp48b1zf7gr8him2jj5dcqgbsl44ls75"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("groff" ,groff)))
    (home-page "https://www.opensmtpd.org")
    (synopsis "Asynchronous resolver library by the OpenBSD project")
    (description
     "libasr is a free, simple and portable asynchronous resolver library.
It allows to run DNS queries and perform hostname resolutions in a fully
asynchronous fashion.")
    (license (list license:isc
                   license:bsd-2 ; last part of getrrsetbyname_async.c
                   license:bsd-3
                   (license:non-copyleft "file://LICENSE") ; includes.h
                   license:openssl))))

(define-public nsd
  (package
    (name "nsd")
    (version "4.1.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.nlnetlabs.nl/downloads/nsd/nsd-"
                           version ".tar.gz"))
       (sha256
        (base32 "1x0mvj4872dzj1rr9adnchdm4dhn41xmc459p5j4s0r13m1l32lz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-pie"             ; fully benefit from ASLR
             "--enable-ratelimit"
             "--enable-recvmmsg"
             "--enable-relro-now"       ; protect GOT and .dtor areas
             "--disable-radix-tree"
             (string-append "--with-libevent="
                            (assoc-ref %build-inputs "libevent"))
             (string-append "--with-ssl="
                            (assoc-ref %build-inputs "openssl"))
             "--with-configdir=/etc"
             "--with-nsd_conf_file=/etc/nsd/nsd.conf"
             "--with-logfile=/var/log/nsd.log"
             "--with-pidfile=/var/db/nsd/nsd.pid"
             "--with-dbfile=/var/db/nsd/nsd.db"
             "--with-zonesdir=/etc/nsd"
             "--with-xfrdfile=/var/db/nsd/xfrd.state"
             "--with-zonelistfile=/var/db/nsd/zone.list")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-installation-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               ;; The ‘make install’ target tries to create the parent
               ;; directories of run-time things like ‘pidfile’ above, and
               ;; useless empty directories like 'configdir'.  Remove such
               ;; '$(INSTALL)' lines and install the example configuration file
               ;; in an appropriate location.
               (substitute* "Makefile.in"
                 ((".*INSTALL.*\\$\\((config|pid|xfr|db)dir" command)
                  (string-append "#" command))
                 (("\\$\\(nsdconfigfile\\)\\.sample" file-name)
                  (string-append doc "/examples/" file-name)))
               #t))))
       #:tests? #f))                    ; no tests
    (inputs
     `(("libevent" ,libevent)
       ("openssl" ,openssl)))
    (home-page "https://www.nlnetlabs.nl/projects/nsd/about/")
    (synopsis "Authoritative DNS name server")
    (description "@dfn{NSD}, short for Name Server Daemon, is an authoritative
name server for the Domain Name System (@dfn{DNS}).  It aims to be a fast and
RFC-compliant nameserver.

NSD uses zone information compiled via @command{zonec} into a binary database
file (@file{nsd.db}).  This allows fast startup of the name service daemon and
allows syntax-structural errors in zone files to be flagged at compile time,
before being made available to NSD service itself.  However, most traditional
BIND-style zone files can be directly imported into NSD without modification.

The collection of programs and processes that make up NSD are designed so that
the daemon itself runs as a non-privileged user and can be easily configured to
run in a @code{chroot} jail, thus making any security flaws in NSD less likely
to result in system-wide compromise.")
    (license (list license:bsd-3))))

(define-public unbound
  (package
    (name "unbound")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.unbound.net/downloads/unbound-"
                           version ".tar.gz"))
       (sha256
        (base32 "05xrb8havr2vgjsdy7n85kgnvk1mg7qwhjp4a8n6pg4jhd5zjnj1"))))
    (build-system gnu-build-system)
    (outputs '("out" "python"))
    (native-inputs
     `(("flex" ,flex)
       ("swig" ,swig)))
    (inputs
     `(("expat" ,expat)
       ("libevent" ,libevent)
       ("protobuf" ,protobuf)
       ("python" ,python-3)
       ("python-wrapper" ,python-wrapper)
       ("openssl" ,openssl)))
    (arguments
     `(#:configure-flags
       (list "--disable-static" ; save space and non-determinism in libunbound.a
             (string-append
              "--with-ssl=" (assoc-ref %build-inputs "openssl"))
             (string-append
              "--with-libevent=" (assoc-ref %build-inputs "libevent"))
             (string-append
              "--with-libexpat=" (assoc-ref %build-inputs "expat"))
             "--with-pythonmodule" "--with-pyunbound")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'fix-python-site-package-path
           ;; Move python modules into their own output.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((pyout (assoc-ref outputs "python"))
                   (ver ,(version-major+minor (package-version python))))
               (substitute* "Makefile"
                 (("^PYTHON_SITE_PKG=.*$")
                  (string-append
                   "PYTHON_SITE_PKG="
                   pyout "/lib/python-" ver "/site-packages\n"))))
             #t))
         (add-before 'check 'fix-missing-nss-for-tests
           ;; Unfortunately, the package's unittests involve some checks
           ;; looking up protocols and services which are not provided
           ;; by the minimalistic build environment, in particular,
           ;; /etc/protocols and /etc/services are missing.
           ;; Also, after plain substitution of protocol and service names
           ;; in the test data, the tests still fail because the
           ;; corresponding Resource Records have been signed by
           ;; RRSIG records.
           ;; The following LD_PRELOAD library overwrites the glibc
           ;; functions ‘get{proto,serv}byname’, ‘getprotobynumber’ and
           ;; ‘getservbyport’ providing the few records required for the
           ;; unit tests to pass.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((source (assoc-ref %build-inputs "source"))
                    (gcc (assoc-ref %build-inputs "gcc")))
               (call-with-output-file "/tmp/nss_preload.c"
                 (lambda (port)
                   (display "#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include <netdb.h>

struct protoent *getprotobyname(const char *name) {
  struct protoent *p = malloc(sizeof(struct protoent));
  p->p_aliases = malloc(sizeof(char*));
  if (strcasecmp(name, \"tcp\") == 0) {
    p->p_name = \"tcp\";
    p->p_proto = 6;
    p->p_aliases[0] = \"TCP\";
  } else if (strcasecmp(name, \"udp\") == 0) {
    p->p_name = \"udp\";
    p->p_proto = 17;
    p->p_aliases[0] = \"UDP\";
  } else 
    p = NULL;
  return p;
}

struct protoent *getprotobynumber(int proto) {
  struct protoent *p = malloc(sizeof(struct protoent));
  p->p_aliases = malloc(sizeof(char*));
  switch(proto) {
  case 6:
    p->p_name = \"tcp\";
    p->p_proto = 6;
    p->p_aliases[0] = \"TCP\";
    break;
  case 17:
    p->p_name = \"udp\";
    p->p_proto = 17;
    p->p_aliases[0] = \"UDP\";
    break;
  default:
    p = NULL;
    break;
  }
  return p;
}

struct servent *getservbyname(const char *name, const char *proto) {
  struct servent *s = malloc(sizeof(struct servent));
  char* buf = malloc((strlen(proto)+1)*sizeof(char));
  strcpy(buf, proto);
  s->s_aliases = malloc(sizeof(char*));
  s->s_aliases[0] = NULL;
  if (strcasecmp(name, \"domain\") == 0) {
    s->s_name = \"domain\";
    s->s_port = htons(53);
    s->s_proto = buf;
  } else 
    s = NULL;
  return s;
}

struct servent *getservbyport(int port, const char *proto) {
  char buf[32];
  struct servent *s = malloc(sizeof(struct servent));
  strcpy(buf, proto);
  s->s_aliases = malloc(sizeof(char*));
  s->s_aliases[0] = NULL;
  switch(port) {
  case 53:
    s->s_name = \"domain\";
    s->s_port = 53;
    s->s_proto = \"udp\";
    break;
  default:
    s = NULL;
    break;
  }
  return s;
}" port)))
               (invoke (string-append gcc "/bin/gcc")
                       "-shared" "-fPIC" "-o" "/tmp/nss_preload.so"
                       "/tmp/nss_preload.c")
               ;; The preload library only affects the unittests.
               (substitute* "Makefile"
                 (("./unittest")
                  "LD_PRELOAD=/tmp/nss_preload.so ./unittest")))
             #t)))))
    (home-page "https://www.unbound.net")
    (synopsis "Validating, recursive, and caching DNS resolver")
    (description
     "Unbound is a recursive-only caching DNS server which can perform DNSSEC
validation of results.  It implements only a minimal amount of authoritative
service to prevent leakage to the root nameservers: forward lookups for
localhost, reverse for @code{127.0.0.1} and @code{::1}, and NXDOMAIN for zones
served by AS112.  Stub and forward zones are supported.")
    (license license:bsd-4)))

(define-public yadifa
  (package
    (name "yadifa")
    (version "2.3.9")
    (source
     (let ((build "8497"))
       (origin
         (method url-fetch)
         (uri
          (string-append "http://cdn.yadifa.eu/sites/default/files/releases/"
                         "yadifa-" version "-" build ".tar.gz"))
         (sha256
          (base32 "0xvyr91sfgzkpw6g3h893ldbwnki3w2472n56rr18w67qghs1sa5")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("openssl" ,openssl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'omit-example-configurations
           (lambda _
             (substitute* "Makefile.in"
               ((" (etc|var)") ""))
             #t)))
       #:configure-flags
       (list "--sysconfdir=/etc"
             "--localstatedir=/var"
             "--disable-build-timestamp" ; build reproducibly
             "--enable-shared"
             "--disable-static"
             "--enable-acl"
             "--enable-caching"
             "--enable-ctrl"            ; enable remote control
             "--enable-nsec"
             "--enable-nsec3"
             "--enable-tsig")))
    (home-page "http://www.yadifa.eu/")
    (synopsis "Authoritative DNS name server")
    (description "YADIFA is an authoritative name server for the @dfn{Domain
Name System} (DNS).  It aims for both higher performance and a smaller memory
footprint than other implementations, while remaining fully RFC-compliant.
YADIFA supports dynamic record updates and the @dfn{Domain Name System Security
Extensions} (DNSSEC).")
    (license license:bsd-3)))

(define-public knot
  (package
    (name "knot")
    (version "2.7.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://secure.nic.cz/files/knot-dns/"
                                  "knot-" version ".tar.xz"))
              (sha256
               (base32
                "18lpyq3vgr2ainmfiy14x7hcf1zxza66bhkpr54jaz2gy1viijx1"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete bundled libraries.
                  (with-directory-excursion "src/contrib"
                    (delete-file-recursively "lmdb"))
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fstrm" ,fstrm)
       ("gnutls" ,gnutls)
       ("jansson" ,jansson)
       ("libcap-ng" ,libcap-ng)
       ("libedit" ,libedit)
       ("libidn" ,libidn)
       ("liburcu" ,liburcu)
       ("lmdb" ,lmdb)
       ("ncurses" ,ncurses)
       ("protobuf-c" ,protobuf-c)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-directory-pre-creation
           (lambda _
             ;; Don't install empty directories like ‘/etc’ outside the store.
             ;; This is needed even when using ‘make config_dir=... install’.
             (substitute* "src/Makefile.in" (("\\$\\(INSTALL\\) -d") "true"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version))
                    (etc (string-append doc "/examples/etc")))
               (invoke "make"
                       (string-append "config_dir=" etc)
                       "install")))))
       #:configure-flags
       (list "--sysconfdir=/etc"
             "--localstatedir=/var"
             "--enable-dnstap"          ; let tools read/write capture files
             "--with-module-dnstap=yes" ; detailed query capturing & logging
             (string-append "--with-bash-completions="
                            (assoc-ref %outputs "out")
                            "/etc/bash_completion.d"))))
    (home-page "https://www.knot-dns.cz/")
    (synopsis "Authoritative DNS name server")
    (description "Knot DNS is an authoritative name server for the @dfn{Domain
Name System} (DNS), designed to meet the needs of root and @dfn{top-level
domain} (TLD) name servers.  It is implemented as a threaded daemon and uses a
number of programming techniques to improve speed.  For example, the responder
is completely lock-free, resulting in a very high response rate.  Other features
include automatic @dfn{DNS Security Extensions} (DNSSEC) signing, dynamic record
synthesis, and on-the-fly re-configuration.")
    (license
     (list
      ;; src/contrib/{hat-trie,murmurhash3,openbsd},
      ;; src/dnssec/contrib/vpool.[ch], and parts of libtap/ are ‘MIT’ (expat).
      license:expat
      license:lgpl2.0+              ; parts of scr/contrib/ucw
      license:public-domain         ; src/contrib/fnv and possibly murmurhash3
      license:gpl3+))))             ; everything else

(define-public ddclient
  (package
    (name "ddclient")
    (version "3.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ddclient/ddclient/ddclient-"
                                  version "/ddclient-" version ".tar.gz"))
              (sha256
               (base32
                "0fwyhab8yga2yi1kdfkbqxa83wxhwpagmj1w1mwkg2iffh1fjjlw"))))
    (build-system trivial-build-system) ; no Makefile.PL
    (native-inputs
     `(("bash" ,bash)
       ("gzip" ,gzip)
       ("perl" ,perl)
       ("tar" ,tar)))
    (inputs
     `(("inetutils" ,inetutils)         ; logger
       ("net-tools" ,net-tools)
       ("perl-data-validate-ip" ,perl-data-validate-ip)
       ("perl-digest-sha1" ,perl-digest-sha1)
       ("perl-io-socket-ssl" ,perl-io-socket-ssl)))
    (arguments
     `(#:modules ((guix build utils)
                  (ice-9 match)
                  (srfi srfi-26))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match)
                      (srfi srfi-26))
         ;; bootstrap
         (setenv "PATH" (string-append
                         (assoc-ref %build-inputs "bash") "/bin" ":"
                         (assoc-ref %build-inputs "tar") "/bin" ":"
                         (assoc-ref %build-inputs "gzip") "/bin" ":"
                         (assoc-ref %build-inputs "perl") "/bin"))
         ;; extract source
         (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
         ;; package
         (with-directory-excursion (string-append ,name "-" ,version)
           (let* ((out (assoc-ref %outputs "out"))
                  (bin (string-append out "/bin")))
             (let ((file "ddclient"))
               (substitute* file
                 (("/usr/bin/perl") (which "perl"))
                 ;; Strictly use ‘/etc/ddclient/ddclient.conf’.
                 (("\\$\\{program\\}\\.conf") "/etc/ddclient/ddclient.conf")
                 (("\\$etc\\$program.conf") "/etc/ddclient/ddclient.conf")
                 ;; Strictly use ‘/var/cache/ddclient/ddclient.cache’
                 (("\\$cachedir\\$program\\.cache")
                  "/var/cache/ddclient/ddclient.cache"))
               (install-file file bin)
               (wrap-program (string-append bin "/" file)
                 `("PATH" ":" =
                   ("$PATH"
                    ,@(map (lambda (input)
                             (match input
                               ((name . store)
                                (string-append store "/bin"))))
                           %build-inputs)))
                 `("PERL5LIB" ":" =
                   ,(delete
                     ""
                     (map (match-lambda
                            (((? (cut string-prefix? "perl-" <>) name) . dir)
                             (string-append dir "/lib/perl5/site_perl"))
                            (_ ""))
                          %build-inputs)))))
             (for-each (cut install-file <> (string-append out
                                                           "/share/ddclient"))
                       (find-files "." "sample.*$")))))))
    (home-page "https://sourceforge.net/projects/ddclient/")
    (synopsis "Address updating utility for dynamic DNS services")
    (description "This package provides a client to update dynamic IP
addresses with several dynamic DNS service providers, such as
@uref{https://www.dyndns.com/account/login.html,DynDNS.com}.

This makes it possible to use a fixed hostname (such as myhost.dyndns.org) to
access a machine with a dynamic IP address.

The client supports both dynamic and (near) static services, as well as MX
record and alternative name management.  It caches the address, and only
attempts the update when it has changed.")
    (license license:gpl2+)))

(define-public hnsd
  ;; There have been no releases yet, hence this commit.
  (let ((revision "0")
        (commit "895d89c25d316d18df9d374fe78aae3902bc89fb"))
   (package
     (name "hnsd")
     (version (git-version "0.0" revision commit))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/handshake-org/hnsd")
                     (commit commit)))
               (sha256
                (base32
                 "0704y73sddn24jga9csw4gxyfb3pnrfnk0vdcph84n1h38490l16"))
               (file-name (git-file-name name version))
               (modules '((guix build utils)))
               (snippet
                '(begin
                   ;; Delete the bundled copy of libuv.
                   (delete-file-recursively "uv")
                   (substitute* "configure.ac"
                     (("AC_CONFIG_SUBDIRS\\(\\[uv\\]\\)") ""))
                   (substitute* "Makefile.am"
                     (("SUBDIRS = uv") "\n")
                     (("\\$\\(top_builddir\\)/uv/libuv.la") "-luv")

                     ;; Make sure the 'hnsd' binary is installed and
                     ;; dynamically-linked.
                     (("noinst_PROGRAMS") "bin_PROGRAMS")
                     (("hnsd_LDFLAGS = -static") ""))

                   ;; This script tries to chdir to "uv" and doesn't do more
                   ;; than "autoreconf" so remove it.
                   (delete-file "autogen.sh")
                   #t))))
     (build-system gnu-build-system)
     (arguments
      '(#:configure-flags '("--disable-static"))) ;no need for libhsk.a
     (native-inputs
      `(("autoconf" ,autoconf)
        ("automake" ,automake)
        ("libtool" ,libtool)))
     (inputs
      `(("unbound" ,unbound)
        ("libuv" ,libuv)))
     (home-page "https://www.handshake.org/")
     (synopsis "Resolver daemon for the Handshake naming protocol")
     (description
      "@command{hnsd} is a @dfn{host name resolver} for the Handshake Naming
System (HNS) peer-to-peer network.")
     (license license:expat))))

(define-public libmicrodns
  (package
    (name "libmicrodns")
    (version "0.0.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/videolabs/libmicrodns")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xvl9k49ng35wbsqmnjnyqvkyjf8dcq2ywsq3jp3wh0rgmxhq2fh"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/videolabs/libmicrodns")
    (synopsis "Minimal mDNS resolver library")
    (description "@code{libmicrodns} provides a minimal implementation of a
mDNS resolver as well as an announcer.  mDNS (Multicast Domain Name System) is
a zero-config service that allows one to resolve host names to IP addresses in
local networks.")
    (license license:lgpl2.1)))
