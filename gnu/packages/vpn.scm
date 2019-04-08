;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Jeff Mickey <j@codemac.net>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
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

(define-module (gnu packages vpn)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public gvpe
  (package
    (name "gvpe")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gvpe/gvpe-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1cz8n75ksl0l908zc5l3rnfm1hv7130s2w8710799fr5sxrdbszi"))))
    (build-system gnu-build-system)
    (home-page "http://software.schmorp.de/pkg/gvpe.html")
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("openssl" ,openssl)
              ("zlib" ,zlib)))
    (synopsis "Secure VPN among multiple nodes over an untrusted network")
    (description
     "The GNU Virtual Private Ethernet creates a virtual network
with multiple nodes using a variety of transport protocols.  It works
by creating encrypted host-to-host tunnels between multiple
endpoints.")
    (license license:gpl3+)))

(define-public vpnc
  (package
   (name "vpnc")
   (version "0.5.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.unix-ag.uni-kl.de/~massar/vpnc/vpnc-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1128860lis89g1s21hqxvap2nq426c9j4bvgghncc1zj0ays7kj6"))))
   (build-system gnu-build-system)
   (inputs `(("libgcrypt" ,libgcrypt)
             ("perl" ,perl)
             ("vpnc-scripts" ,vpnc-scripts)))
   (arguments
    `(#:tests? #f ; there is no check target
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'use-store-paths
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out          (assoc-ref outputs "out"))
                  (vpnc-scripts (assoc-ref inputs  "vpnc-scripts")))
              (substitute* "config.c"
                (("/etc/vpnc/vpnc-script")
                 (string-append vpnc-scripts "/etc/vpnc/vpnc-script")))
              (substitute* "Makefile"
                (("ETCDIR=.*")
                 (string-append "ETCDIR=" out "/etc/vpnc\n"))
                (("PREFIX=.*")
                 (string-append "PREFIX=" out "\n")))
              #t)))
        (delete 'configure))))          ; no configure script
   (synopsis "Client for Cisco VPN concentrators")
   (description
    "vpnc is a VPN client compatible with Cisco's EasyVPN equipment.
It supports IPSec (ESP) with Mode Configuration and Xauth.  It supports only
shared-secret IPSec authentication with Xauth, AES (256, 192, 128), 3DES,
1DES, MD5, SHA1, DH1/2/5 and IP tunneling.  It runs entirely in userspace.
Only \"Universal TUN/TAP device driver support\" is needed in the kernel.")
   (license license:gpl2+) ; some file are bsd-2, see COPYING
   (home-page "http://www.unix-ag.uni-kl.de/~massar/vpnc/")))

(define-public vpnc-scripts
  (let ((commit "1000e0f6dd7d6bff163169a46359211c1fc3a6d2"))
    (package
      (name "vpnc-scripts")
      (version (string-append "20190116." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri
                 (git-reference
                  (url "git://git.infradead.org/users/dwmw2/vpnc-scripts.git")
                  (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1g41yarz2bl0f73kbjqnywr485ghanbp7nmspklfb0n07yp0z6ak"))))
      (build-system gnu-build-system)
      (inputs `(("coreutils" ,coreutils)
                ("grep" ,grep)
                ("iproute2" ,iproute)    ; for ‘ip’
                ("net-tools" ,net-tools) ; for ‘ifconfig’, ‘route’
                ("sed" ,sed)
                ("which" ,which)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'use-relative-paths
             ;; Patch the scripts to work with and use relative paths.
             (lambda* _
               (for-each (lambda (script)
                           (substitute* script
                             (("^PATH=.*") "")
                             (("(/usr|)/s?bin/") "")
                             (("\\[ +-x +([^]]+) +\\]" _ command)
                              (string-append "command -v >/dev/null 2>&1 "
                                             command))))
                         (find-files "." "^vpnc-script"))
               #t))
           (delete 'configure)          ; no configure script
           (replace 'build
             (lambda _
               (invoke "gcc" "-o" "netunshare" "netunshare.c")))
           (replace 'install
             ;; There is no Makefile; manually install the relevant files.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (etc (string-append out "/etc/vpnc")))
                 (for-each (lambda (file)
                             (install-file file etc))
                           (append (find-files "." "^vpnc-script")
                                   (list "netunshare"
                                         "xinetd.netns.conf")))
                 #t)))
           (add-after 'install 'wrap-scripts
             ;; Wrap scripts with paths to their common hard dependencies.
             ;; Optional dependencies will need to be installed by the user.
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (for-each
                  (lambda (script)
                    (wrap-program script
                      `("PATH" ":" prefix
                        ,(map (lambda (name)
                                (let ((input (assoc-ref inputs name)))
                                  (string-append input "/bin:"
                                                 input "/sbin")))
                              (list "coreutils"
                                    "grep"
                                    "iproute2"
                                    "net-tools"
                                    "sed"
                                    "which")))))
                  (find-files (string-append out "/etc/vpnc/vpnc-script")
                              "^vpnc-script"))
                 #t))))
         #:tests? #f))                  ; no tests
      (home-page "http://git.infradead.org/users/dwmw2/vpnc-scripts.git")
      (synopsis "Network configuration scripts for Cisco VPN clients")
      (description
       "This set of scripts configures routing and name services when invoked
by the VPNC or OpenConnect Cisco @dfn{Virtual Private Network} (VPN) clients.

The default @command{vpnc-script} automatically configures most common
connections, and provides hooks for performing custom actions at various stages
of the connection or disconnection process.

Alternative scripts are provided for more complicated set-ups, or to serve as an
example for writing your own.  For example, @command{vpnc-script-sshd} contains
the entire VPN in a network namespace accessible only through SSH.")
      (license license:gpl2+))))

(define-public ocproxy
  (package
    (name "ocproxy")
    (version "1.60")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cernekee/ocproxy/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b4rg3xq5jnrp2l14sw0msan8kqhdxmsd7gpw9lkiwvxy13pcdm7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("libevent" ,libevent)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _ (invoke "sh" "autogen.sh"))))))
    (home-page "https://github.com/cernekee/ocproxy")
    (synopsis "OpenConnect proxy")
    (description
     "User-level @dfn{SOCKS} and port forwarding proxy for OpenConnect based
on LwIP.  When using ocproxy, OpenConnect only handles network activity that
the user specifically asks to proxy, so the @dfn{VPN} interface no longer
\"hijacks\" all network traffic on the host.")
    (license license:bsd-3)))

(define-public openconnect
  (package
   (name "openconnect")
   (version "8.02")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.infradead.org/pub/openconnect/"
                                "openconnect-" version ".tar.gz"))
            (sha256 (base32
                     "04p0vzc1791h68hd9803wsyb64zrwm8qpdqx0szhj9pig71g5a0w"))))
   (build-system gnu-build-system)
   (inputs
    `(("libxml2" ,libxml2)
      ("gnutls" ,gnutls)
      ("vpnc-scripts" ,vpnc-scripts)
      ("zlib" ,zlib)))
   (native-inputs
    `(("gettext" ,gettext-minimal)
      ("pkg-config" ,pkg-config)))
   (arguments
    `(#:configure-flags
      `(,(string-append "--with-vpnc-script="
                        (assoc-ref %build-inputs "vpnc-scripts")
                        "/etc/vpnc/vpnc-script"))))
   (synopsis "Client for Cisco VPN")
   (description
    "OpenConnect is a client for Cisco's AnyConnect SSL VPN, which is
supported by the ASA5500 Series, by IOS 12.4(9)T or later on Cisco SR500,
870, 880, 1800, 2800, 3800, 7200 Series and Cisco 7301 Routers,
and probably others.")
   (license license:lgpl2.1)
   (home-page "https://www.infradead.org/openconnect/")))

(define-public openvpn
  (package
    (name "openvpn")
    (version "2.4.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://swupdate.openvpn.org/community/releases/openvpn-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0j7na936isk9j8nsdrrbw7wmy09inmjqvsb8mw8az7k61xbm6bx4"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-iproute2=yes")))
    (native-inputs
     `(("iproute2" ,iproute)))
    (inputs
     `(("lz4" ,lz4)
       ("lzo" ,lzo)
       ("openssl" ,openssl)
       ("linux-pam" ,linux-pam)))
    (home-page "https://openvpn.net/")
    (synopsis "Virtual private network daemon")
    (description
     "OpenVPN implements virtual private network (@dfn{VPN}) techniques
for creating secure point-to-point or site-to-site connections in routed or
bridged configurations and remote access facilities.  It uses a custom
security protocol that utilizes SSL/TLS for key exchange.  It is capable of
traversing network address translators (@dfn{NAT}s) and firewalls.")
    (license license:gpl2)))

(define-public tinc
  (package
    (name "tinc")
    (version "1.0.35")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://tinc-vpn.org/packages/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pl92sdwrkiwgll78x0ww06hfljd07mkwm62g8x17qn3gha3pj0q"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       '("--sysconfdir=/etc"
         "--localstatedir=/var")))
    (inputs `(("zlib" ,zlib)
              ("lzo" ,lzo)
              ("openssl" ,openssl)))
    (home-page "https://tinc-vpn.org")
    (synopsis "Virtual Private Network (VPN) daemon")
    (description
     "Tinc is a VPN that uses tunnelling and encryption to create a secure
private network between hosts on the internet.")
    (license license:gpl2+)))

(define-public sshuttle
  (package
    (name "sshuttle")
    (version "0.78.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "0vp13xwrhx4m6zgsyzvai84lkq9mzkaw47j58dk0ll95kaymk2x8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-FHS-file-names
           (lambda _
             (substitute* "sshuttle/client.py"
               (("/usr/bin/env") (which "env")))
             (substitute* "sshuttle/ssh.py"
               (("/bin/sh") "sh"))
             #t)))))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)
       ;; For tests only.
       ("python-flake8", python-flake8)
       ("python-mock" ,python-mock)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/sshuttle/sshuttle")
    (synopsis "VPN that transparently forwards connections over SSH")
    (description "sshuttle creates an encrypted virtual private network (VPN)
connection to any remote server to which you have secure shell (SSH) access.
The only requirement is a suitable version of Python on the server;
administrative privileges are required only on the client.  Unlike most VPNs,
sshuttle forwards entire sessions, not packets, using kernel transparent
proxying.  This makes it faster and more reliable than SSH's own tunneling and
port forwarding features.  It can forward both TCP and UDP traffic, including
DNS domain name queries.")
    (license license:lgpl2.0))) ; incorrectly identified as GPL in ‘setup.py’

(define-public sshoot
  (package
    (name "sshoot")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "1ccgh0hjyxrwkgy3hnxz3hgbjbs0lmfs25d5l5jam0xbpcpj63h0"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda _
             (substitute* "sshoot/tests/test_manager.py"
               (("/bin/sh") (which "sh")))
             #t)))))
    (inputs
     `(("python-argcomplete" ,python-argcomplete)
       ("python-prettytable" ,python-prettytable)
       ("python-pyyaml" ,python-pyyaml)))
    ;; For tests only.
    (native-inputs
     `(("python-fixtures" ,python-fixtures)
       ("python-pbr" ,python-pbr)
       ("python-testtools" ,python-testtools)))
    (home-page "https://github.com/albertodonato/sshoot")
    (synopsis "sshuttle VPN session manager")
    (description "sshoot provides a command-line interface to manage multiple
@command{sshuttle} virtual private networks.  It supports flexible profiles
with configuration options for most of @command{sshuttle}’s features.")
    (license license:gpl3+)))

(define-public badvpn
  (package
    (name "badvpn")
    (version "1.999.130")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ambrop72/badvpn.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rm67xhi7bh3yph1vh07imv5y1pwyldvw3wa5bz471g8mnkc7d3c"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no tests
    (inputs
     `(("nspr" ,nspr)
       ("nss" ,nss)
       ("openssl" ,openssl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/ambrop72/badvpn")
    (synopsis "Peer-to-peer virtual private network (VPN)")
    (description "@code{BadVPN} is a collection of virtual private
network (VPN) tools.  It includes:

@enumerate
@item NCD programming language.\n
NCD (Network Configuration Daemon) is a daemon and programming/scripting
language for configuration of network interfaces and other aspects of the
operating system.
@item Tun2socks network-layer proxifier.\n
The tun2socks program socksifes TCP connections at the network layer.  It
implements a TUN device which accepts all incoming TCP connections (regardless
of destination IP), and forwards the connections through a SOCKS server.
@item Peer-to-peer VPN.\n
The peer-to-peer VPN implements a Layer 2 (Ethernet) network between the peers
(VPN nodes).
@end enumerate")
    ;; This project contains a bundled lwIP. lwIP is also released under the
    ;; 3-clause BSD license.
    (license license:bsd-3)))

(define-public wireguard
  (package
    (name "wireguard")
    (version "0.0.20190406")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.zx2c4.com/WireGuard/snapshot/"
                                  "WireGuard-" version ".tar.xz"))
              (sha256
               (base32
                "0ns1s31mfkj7nmapsnx126rj7xlydv7jv8infx5fg58byynz61ig"))))
    (build-system gnu-build-system)
    (outputs '("out" ; The WireGuard userspace tools
               "kernel-patch")) ; A patch to build Linux with WireGuard support
    (arguments
     `(#:tests? #f ; No tests available.
       #:make-flags
       (list "CC=gcc"
             "WITH_BASHCOMPLETION=yes"
             ;; Build and install the helper script wg-quick(8).
             "WITH_WGQUICK=yes"
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "SYSCONFDIR=" (assoc-ref %outputs "out") "/etc"))
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 textual-ports))
       #:phases
       (modify-phases %standard-phases
         ;; There is no ./configure script.
         (delete 'configure)
         ;; Until WireGuard is added to the upstream Linux kernel, it is
         ;; distributed as a kernel patch generated by this script.
         (add-after 'patch-source-shebangs 'make-patch
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((output (string-append (assoc-ref outputs "kernel-patch")
                                           "/wireguard.patch"))
                    (patch-builder "./contrib/kernel-tree/create-patch.sh")
                    (port (open-input-pipe patch-builder))
                    (str (get-string-all port)))
               (close-pipe port)
               (mkdir-p (dirname output))
               (call-with-output-file output
                 (lambda (port)
                   (format port "~a" str))))
               #t))
         (add-after 'make-patch 'chdir
           (lambda _
             (chdir "src/tools")
             #t))
         ;; Otherwise the 'install-license-file' phase installs nothing.
         ;; <https://bugs.gnu.org/34703>
         (add-after 'install 'reset-cwd
           (lambda _
             (chdir "../..")
             #t)))))
    (inputs
     `(("libmnl" ,libmnl)))
    (home-page "https://www.wireguard.com/")
    (synopsis "Tools for configuring WireGuard")
    (description "This package provides the userspace tools for setting and
retrieving configuration of WireGuard network tunnel interfaces, and a patch
that can be applied to a Linux kernel source tree in order to build it with
WireGuard support.")
    (license license:gpl2)))

(define-public xl2tpd
  (package
    (name "xl2tpd")
    (version "1.3.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xelerance/xl2tpd")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1nzkmhi9arwd4smhr07l0sssx46w48z0cblv7xcz25wg4hw86mcd"))
              (file-name (string-append "xl2tpd-" version "-checkout"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "DESTDIR=" %output)
                          "CC=gcc")
       #:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:tests? #f)) ;; no tests provided
    (inputs `(("libpcap" ,libpcap)))
    (home-page "https://www.xelerance.com/software/xl2tpd/")
    (synopsis "Layer 2 Tunnelling Protocol Daemon (RFC 2661)")
    (description
     "xl2tpd is an implementation of the Layer 2 Tunnelling Protocol (RFC 2661).
L2TP allows you to tunnel PPP over UDP.")
    (license license:gpl2)))
