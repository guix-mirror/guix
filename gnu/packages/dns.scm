;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Copyright © 2016, 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
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
  #:use-module (gnu packages databases)
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
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public dnsmasq
  (package
    (name "dnsmasq")
    (version "2.78")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.thekelleys.org.uk/dnsmasq/dnsmasq-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0ar5h5v3kas2qx2wgy5iqin15gc4jhqrqs067xacgc3lii1rz549"))))
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
       ;; No 'check' target.
       #:tests? #f))
    (home-page "http://www.thekelleys.org.uk/dnsmasq/doc.html")
    (synopsis "Small caching DNS proxy and DHCP/TFTP server")
    (description
     "Dnsmasq is a lightweight DNS forwarder and DHCP server.  It is designed
to provide DNS and optionally, DHCP, to a small network.  It can serve the
names of local machines which are not in the global DNS.  The DHCP server
integrates with the DNS server and allows machines with DHCP-allocated
addresses to appear in the DNS with names configured either in each host or in
a central configuration file.  Dnsmasq supports static and dynamic DHCP leases
and BOOTP/TFTP for network booting of diskless machines.")
    ;; Source files only say GPL2 and GPL3 are allowed.
    (license (list license:gpl2 license:gpl3))))

;; 'bind' is the name of a built-in Guile procedure, which is why we choose a
;; different name here.
(define-public isc-bind
  (package
    (name "bind")
    (version "9.11.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.isc.org/isc/bind9/" version "/" name "-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0yn7wgi2y8mpmvbjbkl4va7p0xsnn48m4yjx6ynb1hzp423asikz"))))
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
             (zero? (system* "make" "force-test")))))))
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
               '(delete-file-recursively "libltdl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoreconf
           (lambda _
             ;; Re-generate build files due to unbundling ltdl.
             ;; TODO: Prevent generating new libltdl and building it.
             ;; The system version is still favored and referenced.
             (zero? (system* "autoreconf" "-vif")))))))
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
             (zero? (system* "make" "configure")))))))
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

(define-public unbound
  (package
    (name "unbound")
    (version "1.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.unbound.net/downloads/unbound-"
                           version ".tar.gz"))
       (sha256
        (base32
         "17qwfmlls0w9kpkya3dlpn44b3kr87wsswzg3gawc13hh8yx8ysf"))))
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
       (list "--disable-static" ;save space and non-determinism in libunbound.a
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
               (system* (string-append gcc "/bin/gcc")
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
    (version "2.2.6")
    (source
     (let ((build "7246"))
       (origin
         (method url-fetch)
         (uri
          (string-append "http://cdn.yadifa.eu/sites/default/files/releases/"
                         name "-" version "-" build ".tar.gz"))
         (sha256
          (base32
           "041a35f5jz2wcn8pxk1m7b2qln2wbvj4ddwb0a53lqabl912xi6p")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("openssl" ,openssl)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'omit-example-configurations
                              (lambda _
                                (substitute* "Makefile.in"
                                  ((" (etc|var)") ""))
                                #t)))
       #:configure-flags (list "--sysconfdir=/etc"      "--localstatedir=/var"
                               "--enable-shared"        "--disable-static"
                               "--enable-messages"      "--enable-ctrl"
                               "--enable-nsec"          "--enable-nsec3"
                               "--enable-tsig"          "--enable-caching")))
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
    (version "2.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://secure.nic.cz/files/knot-dns/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "143pk2124liiq1r4ja1s579nbv3hm2scbbfbfclc2pw60r07mcig"))
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
       ("nettle" ,nettle)
       ("protobuf-c" ,protobuf-c)

       ;; For ‘pykeymgr’, needed to migrate keys from versions <= 2.4.
       ("python" ,python-2)
       ("python-lmdb" ,python2-lmdb)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-directory-pre-creation
           (lambda _
             ;; Don't install empty directories like ‘/etc’ outside the store.
             (substitute* "src/Makefile.in" (("\\$\\(INSTALL\\) -d") "true"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/knot"))
                    (etc (string-append doc "/examples/etc")))
               (zero?
                (system* "make"
                         (string-append "config_dir=" etc)
                         "install")))))
         (add-after 'install 'wrap-python-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (path (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/sbin/pykeymgr")
                 `("PYTHONPATH" ":" prefix (,path))))
             #t)))
       #:configure-flags
       (list "--sysconfdir=/etc"
             "--localstatedir=/var"
             "--with-module-rosedb=yes" ; serve static records from a database
             "--with-module-dnstap=yes" ; allow detailed query logging
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
