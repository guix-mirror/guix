;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2018, 2022 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2020, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Simon South <simon@simonsouth.net>
;;; Copyright © 2021 Zheng Junjie <873216071@qq.com>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial))

(define-public cloudflare-cli
  (let ((commit "2d986d3ec1b0e3158c4bd40e8918947cb74aa392")
        (revision "1"))
    (package
      (name "cloudflare-cli")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/earlchew/cloudflare-cli")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0f86g6n86kwykl3jnhqjrdfy8ybkp03ghr3dlr70q2552qw4axw2"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan '(("cloudflare-cli" "bin/") ("cloudflare-cli.sh" "bin/"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'find-jsonsh
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "cloudflare-cli.sh"
                 (("\\$\\{0%/\\*\\}/jsonsh")
                  (string-append (assoc-ref inputs "json.sh") "/bin/JSON.sh")))
               #t))
           (add-after 'install 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (wrap-program (string-append (assoc-ref outputs "out") "/bin/cloudflare-cli")
                 `("PATH" ":" prefix
                   (,(string-join
                      (map (lambda (in) (string-append (assoc-ref inputs in) "/bin"))
                           '("grep" "curl"))
                      ":"))))
               #t)))))
      (inputs
       (list bash-minimal curl grep json.sh))
      (synopsis
        "CLI to edit Cloudflare DNS records")
      (description
        "This command line tool to update Cloudfare DNS records is useful for tasks
such as updating dynamic DNS records or updating DNS records for the ACME DNS-01
protocol.")
      (home-page "https://github.com/earlchew/cloudflare-cli")
      (license license:expat))))

(define-public ldns
  (package
    (name "ldns")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.nlnetlabs.nl/downloads/"
                       "ldns/ldns-" version ".tar.gz"))
       (sha256
        (base32 "18vzdmyg9bm45janw602d4hifjsncrv143awlwcslfjdrsmjk0lm"))
       (patches
        (search-patches
         ;; To create make-flag variables,
         ;; for splitting installation of drill and examples.
         "ldns-drill-examples.patch"))))
    (build-system gnu-build-system)
    (outputs '("out" "drill" "examples" "pyldns"))
    (arguments
     (list
      #:tests? #f                     ; tests require <https://tpkg.github.io>
      #:configure-flags
      #~(list
         "--disable-static"
         "--enable-gost-anyway"
         "--enable-rrtype-ninfo"
         "--enable-rrtype-rkey"
         "--enable-rrtype-ta"
         "--enable-rrtype-avc"
         "--enable-rrtype-doa"
         "--enable-rrtype-amtrelay"
         "--with-drill"
         "--with-examples"
         "--with-pyldns"
         ;; Perl module DNS::LDNS not available.
         ;; https://github.com/erikoest/DNS-LDNS.git
         ;; "--with-p5-dns-ldns"
         (string-append "--with-ssl=" #$(this-package-input "openssl"))
         (string-append "--with-ca-path=/etc/ssl/certs"))
      #:make-flags
      #~(list
         (string-append "drillbindir=" #$output:drill "/bin")
         (string-append "drillmandir=" #$output:drill "/share/man")
         (string-append "examplesbindir=" #$output:examples "/bin")
         (string-append "examplesmandir=" #$output:examples "/share/man")
         (string-append "python_site=" #$output:pyldns "/lib/python"
                        #$(version-major+minor (package-version
                                                (this-package-input
                                                 "python-wrapper")))
                        "/site-packages"))))
    (native-inputs
     (list doxygen perl perl-devel-checklib pkg-config swig))
    (inputs
     (list libpcap openssl python-wrapper))
    (synopsis "DNS library that facilitates DNS tool programming")
    (description "LDNS aims to simplify DNS programming, it supports recent
RFCs like the DNSSEC documents, and allows developers to easily create
software conforming to current RFCs, and experimental software for current
Internet Drafts.  A secondary benefit of using ldns is speed; ldns is written in
C it should be a lot faster than Perl.")
    (home-page "https://nlnetlabs.nl/projects/ldns/about/")
    (license license:bsd-3)))

(define-public dnssec-trigger
  (package
    (name "dnssec-trigger")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.nlnetlabs.nl/downloads/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "10928q406x9r66a090xl5kznzgyxpja88w4srwcv454hd351j9f0"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "gui" "nm"))
    (arguments
     (list #:test-target "test"
           #:configure-flags
           #~(list
              (string-append "--with-ssl=" #$(this-package-input "openssl"))
              "--with-hooks=networkmanager"
              (string-append "--with-networkmanager-dispatch="
                             #$output:nm
                             "/etc/NetworkManager/dispatcher.d")
              (string-append "--with-xdg-autostart="
                             #$output:gui
                             "/etc/xdg/autostart")
              (string-append "--with-uidir="
                             #$output:gui
                             "/share/dnssec-trigger")
              (string-append "--with-python="
                             #$(this-package-native-input "python-wrapper")
                             "/bin/python")
              (string-append "--with-unbound-control="
                             #$(this-package-input "unbound")
                             "/sbin/unbound-control")
              "--with-forward-zones-support")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-configure
                 (lambda _
                   (substitute* "configure"
                     (("appindicator-0.1")
                      "appindicator3-0.1"))))
               (add-before 'configure 'patch-makefile
                 (lambda _
                   (substitute* "Makefile.in"
                     (("/usr")
                      "$(prefix)")
                     (("/etc")
                      "$(prefix)/etc")
                     ((".*gtk-update-icon-cache.*")
                      ""))))
               (add-after 'install 'remove-systemd
                 (lambda _
                   (delete-file-recursively
                    (string-append #$output "/lib/systemd"))))
               (add-after 'remove-systemd 'move-gui
                 (lambda _
                   (mkdir-p (string-append #$output:gui "/bin"))
                   (mkdir-p (string-append #$output:gui "/share"))
                   (rename-file
                    (string-append #$output     "/bin")
                    (string-append #$output:gui "/bin"))
                   (rename-file
                    (string-append #$output     "/share/icons")
                    (string-append #$output:gui "/share/icons"))))
               (add-after 'move-gui 'move-nm
                 (lambda _
                   (mkdir-p (string-append #$output:nm "/libexec"))
                   (rename-file
                    (string-append #$output    "/libexec")
                    (string-append #$output:nm "/libexec")))))))
    (native-inputs
     (list cmocka pkg-config python-wrapper))
    (inputs
     (list gtk+-2 ldns libappindicator openssl unbound))
    (synopsis "DNSSEC protection for the DNS traffic")
    (description "DNSSEC-Trigger enables your computer to use DNSSEC protection
for the DNS traffic.  It relies on the Unbound DNS resolver running locally on
your system, which performs DNSSEC validation.  It reconfigures Unbound in such
a way that it will signal it to to use the DHCP obtained forwarders if possible,
fallback to doing its own AUTH queries if that fails, and if that fails it will
prompt the user with the option to go with insecure DNS only.")
    (home-page "https://www.nlnetlabs.nl/projects/dnssec-trigger/about/")
    (license license:bsd-3)))

(define-public dnsmasq
  (package
    (name "dnsmasq")
    (version "2.86")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.thekelleys.org.uk/dnsmasq/dnsmasq-"
                    version ".tar.xz"))
              (sha256
               (base32
                "027b0ycw8h8yvvkq46vnr7dv8iqn5srm4kr7hm7sq110kvy2rm98"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list dbus))
    (arguments
     `(#:phases
       (modify-phases %standard-phases (delete 'configure))
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          (string-append "CC=" ,(cc-for-target))
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
    ;; When updating, check whether isc-dhcp's bundled copy should be as well.
    ;; The BIND release notes are available here:
    ;; https://www.isc.org/bind/
    (version "9.16.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://ftp.isc.org/isc/bind9/" version
                           "/bind-" version ".tar.xz"))
       (sha256
        (base32 "1wqzbq7jfd8zlidkfgx3fc1132xn5hrga7xznzw3x1421y2ji8wz"))
       (patches
        (search-patches "bind-re-add-attr-constructor-priority.patch"))))
    (build-system gnu-build-system)
    (outputs `("out" "utils"))
    (inputs
     ;; It would be nice to add GeoIP and gssapi once there are packages.
     (list libcap
           libuv
           libxml2
           openssl
           p11-kit
           python
           python-ply))
    (native-inputs
     (list perl pkg-config))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-pkcs11="
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
                "/share/man/man1/nsupdate.1"))
             #t))
         ;; When and if guix provides user namespaces for the build process,
         ;; then the following can be uncommented and the subsequent "force-test"
         ;; will not be necessary.
         ;;
         ;;   (add-before 'check 'set-up-loopback
         ;;     (lambda _
         ;;          (system "bin/tests/system/ifconfig.sh up")))
         (replace 'check
           (lambda _
             ;; XXX Even ‘make force-test’ tries to create network interfaces
             ;; and fails.  The only working target is the (trivial) fuzz test.
             (with-directory-excursion "fuzz"
               (invoke "make" "check"))
             #t)))))
    (synopsis "@acronym{DNS, Domain Name System} implementation")
    (description "BIND implements the @acronym{DNS, Domain Name System}
protocols for the Internet.  It is both a reference implementation of those
protocols and production-grade software, suitable for use in high-volume and
high-reliability applications.

The name stands for \"Berkeley Internet Name Domain\" because the software
originated in the early 1980s at the University of California at Berkeley.

The @code{utils} output of this package contains the following command line
utilities related to DNS name servers:

@table @code
@item delv
DNS lookup and validation utility
@item dig
DNS lookup utility
@item host
DNS lookup utility
@item nslookup
Internet name servers interactive query utility
@item nsupdate
Dynamic DNS update utility
@end table")
    (home-page "https://www.isc.org/bind/")
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
     (list pkg-config automake autoconf libtool))
    (inputs
     (list libltdl libsodium))
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
     (list autoconf))
    (inputs
     (list libevent libsodium))
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
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.opensmtpd.org/archives/"
                           "libasr-" version ".tar.gz"))
       (sha256
        (base32 "1d6s8njqhvayx2gp47409sp1fn8m608ws26hr1srfp6i23nnpyqr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "src/asr_run.3"
                             (string-append out "/share/man/man3"))
               #t))))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://www.opensmtpd.org")
    (synopsis "Asynchronous resolver library by the OpenBSD project")
    (description
     "libasr is a free, simple and portable asynchronous resolver library.
It runs DNS queries and performs hostname resolution in a fully
asynchronous fashion.")
    (license (list license:isc
                   license:bsd-2 ; last part of getrrsetbyname_async.c
                   license:bsd-3
                   (license:non-copyleft "file://LICENSE") ; includes.h
                   license:openssl))))

(define-public nsd
  (package
    (name "nsd")
    (version "4.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.nlnetlabs.nl/downloads/nsd/nsd-"
                           version ".tar.gz"))
       (sha256
        (base32 "13ay2gr7ln8gl09wdqnxkrdxi51jaqsbn54yh82vvv49jbq4j5ak"))))
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
                  (string-append doc "/examples/" file-name)))))))
       #:tests? #f))                    ; no tests
    (inputs
     (list libevent openssl))
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

(define-public rbldnsd
  (package
    (name "rbldnsd")
    (version "0.998b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spamhaus/rbldnsd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jj3kyir43qnjgd9rk0wz13iggf3p4p1779v0wgmx3ci0ypnglcr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; The ./configure is hand-written and doesn't ignore unknown
           ;; standard autotools options like CONFIG_SHELL.
           (lambda _
             (invoke "./configure")))
         (replace 'install
           ;; There is no Makefile ‘install’ target.  contrib/debian/rules has
           ;; one but relies on Debian-specific helpers, so install manually.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin"))
                    (man8 (string-append out "/share/man/man8")))
               (install-file "rbldnsd" sbin)
               (install-file "rbldnsd.8" man8)))))))
    (inputs
     (list zlib))
    (native-inputs
     ;; For running the test suite.  Python 3 is not yet supported by a release:
     ;; see <https://github.com/spamhaus/rbldnsd/issues/16>.
     `(("python" ,python-2)))
    (home-page "https://rbldnsd.io/")
    (synopsis
     "Small nameserver to efficiently serve @acronym{DNSBL, DNS blocklists}")
    (description
     "This package contains a small DNS daemon especially made to handle queries
of @acronym{DNSBL, DNS blocklists}, a simple way to publish IP addresses and/or
(domain) names which are somehow notable.  Such lists are frequently used to
refuse e-mail service to clients known to send unwanted (spam) messages.

@command{rbldnsd} is not a general-purpose nameserver.  It answers to a limited
variety of queries.  This makes it extremely fast---greatly outperforming both
BIND and djbdns---whilst using relatively little memory.")
    (license
     (list license:bsd-3                ; btrie.[ch]
           license:lgpl2.1+             ; qsort.c
           license:gpl2+))))            ; the rest

(define-public unbound
  (package
    (name "unbound")
    (version "1.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.unbound.net/downloads/unbound-"
                           version ".tar.gz"))
       (sha256
        (base32 "10qs1q26lzw18ljggnbz0cc5f7lr9ksj615xbrmh4amryd3va4qa"))))
    (build-system gnu-build-system)
    (outputs '("out" "python"))
    (native-inputs
     (list flex swig))
    (inputs
     (list expat
           libevent
           `(,nghttp2 "lib")
           protobuf
           python-wrapper
           openssl))
    (arguments
     `(#:configure-flags
       (list "--disable-static" ; save space and non-determinism in libunbound.a
             (string-append
              "--with-libnghttp2=" (assoc-ref %build-inputs "nghttp2"))
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
    (version "2.5.3")
    (source
     (let ((build "10333"))
       (origin
         (method url-fetch)
         (uri
          (string-append "https://www.yadifa.eu/sites/default/files/releases/"
                         "yadifa-" version "-" build ".tar.gz"))
         (sha256
          (base32 "1mwy6sfnlaslx26f3kpj9alh8i8y8bf1nbnsdd5j04hjsbavd07p")))))
    (build-system gnu-build-system)
    (native-inputs
     (list which))
    (inputs
     (list openssl))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'omit-example-configurations
           (lambda _
             (substitute* "Makefile.in"
               ((" (etc|var)") ""))))
         (add-after 'configure 'omit-spurious-references
           (lambda _
             ;; The many Makefile.in grep this(!) to #define BUILD_OPTIONS.
             (substitute* "config.log"
               (("(=/gnu/store/)[^-]*" _ match)
                (string-append match "..."))))))
       #:configure-flags
       (list "--sysconfdir=/etc"
             "--localstatedir=/var"
             "--enable-shared" "--disable-static"
             "--disable-build-timestamp"    ; build reproducibly
             "--enable-tcp-manager")))
    (home-page "https://www.yadifa.eu/")
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
    (version "3.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.nic.cz/knot/knot-dns")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "145fnz740y1g0h2m07kpcimf2rx37saq2l905bl6vwa5ifybrgcq"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove Ragel-generated C files.  We'll recreate them below.
           (for-each delete-file (find-files "." "\\.c\\.[gt]."))
           (delete-file "src/libknot/yparser/ypbody.c")
           ;; Remove bundled library to ensure we always use the system's.
           (delete-file-recursively "src/contrib/libbpf")))))
    (build-system gnu-build-system)
    (outputs (list "out" "doc" "lib" "tools"))
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "doc")
                            "/share/" ,name "-" ,version)
             (string-append "--infodir=" (assoc-ref %outputs "doc")
                            "/share/info")
             (string-append "--libdir=" (assoc-ref %outputs "lib") "/lib")
             "--sysconfdir=/etc"
             "--localstatedir=/var"
             "--disable-static"         ; static libraries are built by default
             "--enable-dnstap"          ; let tools read/write capture files
             "--enable-fastparser"      ; disabled by default when .git/ exists
             "--enable-xdp=yes"
             "--with-module-dnstap=yes") ; detailed query capturing & logging
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-missing-libbpf-dependency
           ;; Linking against -lbpf later would fail to find -lz: libbpf.pc has
           ;; zlib in its Requires.private (not Requires) field.  Add it here.
           (lambda _
             (substitute* "configure.ac"
               (("enable_xdp=yes" match)
                (string-append match "\nlibbpf_LIBS=\"$libbpf_LIBS -lz\"")))))
         (add-before 'bootstrap 'update-parser
           (lambda _
             (with-directory-excursion "src"
               (invoke "sh" "../scripts/update-parser.sh"))))
         (add-before 'configure 'disable-directory-pre-creation
           (lambda _
             ;; Don't install empty directories like ‘/etc’ outside the store.
             ;; This is needed even when using ‘make config_dir=... install’.
             (substitute* "src/Makefile.in" (("\\$\\(INSTALL\\) -d") "true"))))
         (add-after 'build 'build-info
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make" "info"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags))))
         (replace 'install
           (lambda* (#:key make-flags outputs parallel-build? #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version))
                    (etc (string-append doc "/examples/etc")))
               (apply invoke "make" "install"
                      (string-append "config_dir=" etc)
                      `(,@(if parallel-build?
                              `("-j" ,(number->string (parallel-job-count)))
                              '())
                        ,@make-flags)))))
         (add-after 'install 'install-info
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make" "install-info"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags))))
         (add-after 'install 'break-circular-:lib->:out-reference
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (assoc-ref outputs "lib")))
               (for-each (lambda (file)
                           (substitute* file
                             (("(prefix=).*" _ assign)
                              (string-append assign lib "\n"))))
                         (find-files lib "\\.pc$")))))
         (add-after 'install 'split-:tools
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (tools (assoc-ref outputs "tools")))
               (mkdir-p (string-append tools "/share/man"))
               (rename-file (string-append out   "/bin")
                            (string-append tools "/bin"))
               (rename-file (string-append out   "/share/man/man1")
                            (string-append tools "/share/man/man1"))))))))
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           python-sphinx
           ragel
           texinfo))
    (inputs
     `(("fstrm" ,fstrm)
       ("gnutls" ,gnutls)
       ("jansson" ,jansson)
       ("libbpf" ,libbpf)
       ("libcap-ng" ,libcap-ng)
       ("libedit" ,libedit)
       ("libelf" ,libelf)
       ("libidn" ,libidn)
       ("libmnl" ,libmnl)
       ("libnghttp2" ,nghttp2 "lib")
       ("liburcu" ,liburcu)
       ("lmdb" ,lmdb)
       ("ncurses" ,ncurses)
       ("protobuf-c" ,protobuf-c)))
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

(define-public knot-resolver
  (package
    (name "knot-resolver")
    (version "5.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://secure.nic.cz/files/knot-resolver/"
                                  "knot-resolver-" version ".tar.xz"))
              (sha256
               (base32
                "1sic5ccbbqml4c01dbikkg6qx1gg81nqi76cj79pjdllkqqn92aq"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     '(#:configure-flags '("-Ddoc=enabled")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-default-ta
           (lambda _
             ;;  Disable the default managed root TA, since we don't have
             ;;  write access to the keyfile and its directory in store.
             (substitute* "daemon/lua/sandbox.lua.in"
               (("^trust_anchors\\.add_file.*") ""))))
         (add-after 'build 'build-doc
           (lambda _
             (invoke "ninja" "doc")))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move the manual and the example configuration files to the
             ;; "doc" output.
             (let ((out (assoc-ref outputs "out"))
                   (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share/doc/knot-resolver"))
               (for-each
                (lambda (dir)
                  (rename-file (string-append out "/share/" dir)
                               (string-append doc "/share/" dir)))
                '("doc/knot-resolver/examples"
                  "doc/knot-resolver/html"
                  "info")))))
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lua-* (map cdr (filter
                                     (lambda (input)
                                       (string-prefix? "lua-" (car input)))
                                     inputs)))
                    (lua-path (lambda (p)
                                (string-append p "/share/lua/5.1/?.lua")))
                    (lua-cpath (lambda (p)
                                 (string-append p "/lib/lua/5.1/?.so"))))
               (wrap-program (string-append out "/sbin/kresd")
                 `("LUA_PATH" ";" prefix ,(map lua-path lua-*))
                 `("LUA_CPATH" ";" prefix ,(map lua-cpath lua-*)))))))))
    (native-inputs
     (list cmocka ; for unit tests
           doxygen
           protobuf-c
           pkg-config
           python-breathe
           python-sphinx
           python-sphinx-rtd-theme
           texinfo))
    (inputs
     `(("fstrm" ,fstrm)
       ("gnutls" ,gnutls)
       ("knot:lib" ,knot "lib")
       ("libuv" ,libuv)
       ("lmdb" ,lmdb)
       ("luajit" ,luajit)
       ;; TODO: Add optional lua modules: basexx and psl.
       ("lua-bitop" ,lua5.1-bitop)
       ("nghttp2" ,nghttp2 "lib")
       ("python" ,python)))
    (home-page "https://www.knot-resolver.cz/")
    (synopsis "Caching validating DNS resolver")
    (description
     "Knot Resolver is a caching full resolver implementation written in C and
LuaJIT, both a resolver library and a daemon.")
    (license (list license:gpl3+
                   ;; Some 'contrib' files are under MIT, CC0 and LGPL2.
                   license:expat
                   license:cc0
                   license:lgpl2.0))))

(define-public ddclient
  (package
    (name "ddclient")
    (version "3.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ddclient/ddclient")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hf377g4j9r9sac75xp17nk2h58mazswz4vkg4g2gl2yyhvzq91w"))))
    (build-system trivial-build-system) ; no Makefile.PL
    (native-inputs
     (list bash perl))
    (inputs
     (list inetutils ; logger
           net-tools
           perl-data-validate-ip
           perl-digest-sha1
           perl-io-socket-ssl))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match)
                      (srfi srfi-26))
         (setenv "PATH" (string-append
                         (assoc-ref %build-inputs "bash") "/bin" ":"
                         (assoc-ref %build-inputs "perl") "/bin"))

         ;; Copy the (read-only) source into the (writable) build directory.
         (copy-recursively (assoc-ref %build-inputs "source") ".")

         ;; Install.
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
                     (find-files "." "sample.*$"))))))
    (home-page "https://ddclient.net/")
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
   (package
     (name "hnsd")
     (version "1.0.0")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/handshake-org/hnsd")
                     (commit (string-append "v" version))))
               (sha256
                (base32
                 "1kdgff8rf8gmvwz2p758ilbjxpvz4xm6z41pa5353asg6xb853bb"))
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
      (list autoconf automake libtool))
     (inputs
      (list unbound libuv))
     (home-page "https://www.handshake.org/")
     (synopsis "Resolver daemon for the Handshake naming protocol")
     (description
      "@command{hnsd} is a @dfn{host name resolver} for the Handshake Naming
System (HNS) peer-to-peer network.")
     (license license:expat)))

(define-public libmicrodns
  (package
    (name "libmicrodns")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/videolabs/libmicrodns/"
                                  "releases/download/" version "/microdns-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0p4va18zxgmzcdwhlbg2mmjwswlbgqy4ay5vaxrw7cxmhsflnv36"))))
    (build-system meson-build-system)
    (home-page "https://github.com/videolabs/libmicrodns")
    (synopsis "Minimal mDNS resolver library")
    (description "@code{libmicrodns} provides a minimal implementation of a
mDNS resolver as well as an announcer.  mDNS (Multicast Domain Name System) is
a zero-config service that allows one to resolve host names to IP addresses in
local networks.")
    (license license:lgpl2.1)))

(define-public public-suffix-list
  ;; Mozilla releases the official list here:
  ;;
  ;;   https://publicsuffix.org/list/public_suffix_list.dat
  ;;
  ;; However, Mozilla syncs that file from the GitHub repository periodically,
  ;; so its contents will change over time.  If you update this commit, please
  ;; make sure that the new commit refers to a list which is identical to the
  ;; officially published list available from the URL above.
  (let ((commit "9375b697baddb0827a5995c81bd3c75877a0b35d"))
    (package
      (name "public-suffix-list")
      (version (git-version "0" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/publicsuffix/list")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1sm7pni01rnl4ldzi8z8nc4cbgq8nxda9gwc68v0s3ij7jd1jmik"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out (assoc-ref %outputs "out"))
                  ;; Install to /share because that is where "read-only
                  ;; architecture-independent data files" should go (see:
                  ;; (standards) Directory Variables).  Include the version in
                  ;; the directory name so that if multiple versions are ever
                  ;; installed in the same profile, they will not conflict.
                  (destination (string-append
                                out "/share/public-suffix-list-" ,version))
                  (source (assoc-ref %build-inputs "source")))
             (with-directory-excursion source
             (install-file "public_suffix_list.dat" destination)
             (install-file "LICENSE" destination))
             #t))))
      (home-page "https://publicsuffix.org/")
      (synopsis "Database of current and historical DNS suffixes")
      (description "This is the Public Suffix List maintained by Mozilla.  A
\"public suffix\" is one under which Internet users can (or historically
could) directly register names in the Domain Name System (DNS).  Some examples
of public suffixes are .com, .co.uk and pvt.k12.ma.us.  This is a list of all
known public suffixes.")
      (license license:mpl2.0))))

(define-public maradns
  (package
    (name "maradns")
    (version "3.5.0020")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://maradns.samiam.org/download/"
                           (version-major+minor version) "/"
                           version "/maradns-" version ".tar.xz"))
       (sha256
        (base32 "1qgabw6y2bwy6y88dikis62k789i0xh7iwxan8jmqpzvksqwjfgw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; need to be root to run tests
       #:make-flags
       (list
        ,(string-append "CC=" (cc-for-target))
        (string-append "PREFIX=" %output)
        (string-append "RPM_BUILD_ROOT=" %output))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key native-inputs target #:allow-other-keys)
             ;; make_32bit_tables generates a header file that is used during
             ;; compilation. Hence, during cross compilation, it should be
             ;; built for the host system.
             (when target
               (substitute* "rng/Makefile"
                 (("\\$\\(CC\\) -o make_32bit_tables")
                  (string-append (assoc-ref native-inputs "gcc")
                                 "/bin/gcc -o make_32bit_tables"))))
             (invoke "./configure")))
         (add-before 'install 'create-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (dir)
                           (mkdir-p (string-append out dir)))
                         (list "/bin" "/sbin" "/etc"
                               "/share/man/man1"
                               "/share/man/man5"
                               "/share/man/man8"))
               #t))))))
    (home-page "https://maradns.samiam.org")
    (synopsis "Small lightweight DNS server")
    (description "MaraDNS is a small and lightweight DNS server.  MaraDNS
consists of a UDP-only authoritative DNS server for hosting domains, and a UDP
and TCP-capable recursive DNS server for finding domains on the internet.")
    (license license:bsd-2)))

(define-public openresolv
  (package
    (name "openresolv")
    (version "3.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://roy.marples.name/downloads/openresolv/"
                                  "openresolv-" version ".tar.xz"))
              (sha256
               (base32
                "15qvp5va2yrqpz0ba54clvn8cbc66v4sl7k3bi9ji8jpx040bcs2"))
              (patches
               (search-patches "openresolv-restartcmd-guix.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No test suite
       #:configure-flags
       (list (string-append "--sysconfdir=/etc"))
       #:make-flags
       (list (string-append "SYSCONFDIR=/" (assoc-ref %outputs "out") "/etc"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (coreutils (assoc-ref inputs "coreutils-minimal")))
               (substitute* (string-append out "/sbin/resolvconf")
                 (("RESOLVCONF=\"\\$0\"")
                  (format #f "\
RESOLVCONF=\"$0\"
PATH=~a/bin:$PATH"
                          coreutils)))))))))
    (inputs
     (list coreutils-minimal))
    (home-page "https://roy.marples.name/projects/openresolv/")
    (synopsis "Resolvconf POSIX compliant implementation, a middleman for resolv.conf")
    (description "openresolv is an implementation of @command{resolvconf}, the
middleman between the network configuration services and
@file{/etc/resolv.conf}.  @command{resolvconf} itself is just a script that
stores, removes and lists a full @file{resolv.conf} generated for the
interface.  It then calls all the helper scripts it knows about so it can
configure the real @file{/etc/resolv.conf} and optionally any local
nameservers other than libc.")
    (license license:bsd-2)))
