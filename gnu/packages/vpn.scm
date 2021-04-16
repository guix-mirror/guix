;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2016, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Jeff Mickey <j@codemac.net>
;;; Copyright © 2016, 2017, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Ivan Kozlov <kanichos@yandex.ru>
;;; Copyright © 2020 David Dashyan <mail@davie.li>
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
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages samba)
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
   (home-page "https://www.unix-ag.uni-kl.de/~massar/vpnc/")))

(define-public vpnc-scripts
  (let ((commit "3885f8bbc4ae03fd6da0ada6de12f7223a59595c"))
    (package
      (name "vpnc-scripts")
      (version (string-append "20200925." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri
                 (git-reference
                  (url "git://git.infradead.org/users/dwmw2/vpnc-scripts.git")
                  (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pmi4n58q81pmn9arvfixhvv6vkkf3rpwac3hwnwyl882q5q0ccx"))))
      (build-system gnu-build-system)
      (inputs `(("guile" ,guile-3.0) ; for the wrapper scripts
                ("coreutils" ,coreutils)
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
                             (("/usr/s?bin/") "")
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
                    (wrap-script (string-append out "/etc/vpnc/" script)
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
                  (list "vpnc-script-ptrtd"
                        "vpnc-script-sshd"
                        "vpnc-script"))
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
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cernekee/ocproxy")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03323nnhb4y9nzwva04mq7xg03dvdrgp689g89f69jqc261skcqx"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("libevent" ,libevent)))
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
   (version "8.10")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.infradead.org/pub/openconnect/"
                                "openconnect-" version ".tar.gz"))
            (sha256
             (base32 "1cdsx4nsrwawbsisfkldfc9i4qn60g03vxb13nzppr2br9p4rrih"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("libxml2" ,libxml2)
      ("gnutls" ,gnutls)
      ("zlib" ,zlib)))
   (inputs
    `(("lz4" ,lz4)
      ("vpnc-scripts" ,vpnc-scripts)))
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

(define-public openconnect-sso
  (package
    (name "openconnect-sso")
    (version "0.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "openconnect-sso" version))
        (sha256
         (base32
          "0nb40zfpp38mz6389y0qvrr4mmak53swpg7578cldnhnk0g15qni"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; Tests not included, building from git requires poetry.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-openconnect
           (lambda _
             (substitute* "openconnect_sso/app.py"
               (("\"openconnect\"")
                (string-append "\"" (which "openconnect") "\"")))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-v"))
             #t))
         (add-after 'install 'wrap-qt-process-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/openconnect-sso"))
                    (qt-process-path (string-append
                                       (assoc-ref inputs "qtwebengine")
                                       "/lib/qt5/libexec/QtWebEngineProcess")))
               (wrap-program bin
                 `("QTWEBENGINEPROCESS_PATH" = (,qt-process-path)))
               #t))))))
    (inputs
     `(("openconnect" ,openconnect)
       ("python-attrs" ,python-attrs)
       ("python-colorama" ,python-colorama)
       ("python-keyring" ,python-keyring)
       ("python-lxml" ,python-lxml)
       ("python-prompt-toolkit" ,python-prompt-toolkit)
       ("python-requests" ,python-requests)
       ("python-pyqt" ,python-pyqt)
       ("python-pyqtwebengine" ,python-pyqtwebengine)
       ("python-pysocks" ,python-pysocks)
       ("python-pyxdg" ,python-pyxdg)
       ("python-structlog" ,python-structlog)
       ("python-toml" ,python-toml)
       ("qtwebengine" ,qtwebengine)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/vlaci/openconnect-sso")
    (synopsis "OpenConnect wrapper script supporting Azure AD (SAMLv2)")
    (description
     "This package provides a wrapper script for OpenConnect supporting Azure AD
(SAMLv2) authentication to Cisco SSL-VPNs.")
    (license license:gpl3)))

(define-public openfortivpn
  (package
    (name "openfortivpn")
    (version "1.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/adrienverge/openfortivpn")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qsfgpxg553s8rc9cyrc4k96z0pislxsdxb9wyhp8fdprkak2mw2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("autotools" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)
       ("ppp" ,ppp)))
    (home-page "https://github.com/adrienverge/openfortivpn")
    (synopsis "Client for PPP+SSL VPN tunnel services")
    (description "Openfortivpn is a client for PPP+SSL VPN tunnel services.  It
spawns a pppd process and operates the communication between the gateway and
this process.  It is compatible with Fortinet VPNs.")
    (license license:gpl3+)))

(define-public openvpn
  (package
    (name "openvpn")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://swupdate.openvpn.org/community/releases/openvpn-"
                    version ".tar.xz"))
              (sha256
               (base32
                "157ikzap2bbkzx34hkh33bpk2i14xjx1x3pkadhmzh1pr24h94s0"))))
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

(define-public protonvpn-cli
  (package
    (name "protonvpn-cli")
    (version "2.2.6")
    (source
     (origin
       ;; PyPI has a ".whl" file but not a proper source release.
       ;; Thus, fetch code from Git.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonVPN/linux-cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y7v9ikrmy5dbjlpbpacp08gy838i8z54m8m4ps7ldk1j6kyia3n"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; no tests in repo
       #:phases
       (modify-phases %standard-phases
         (add-after 'wrap 'wrap-wrapper
           ;; Wrap entrypoint with paths to its hard dependencies.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((entrypoint (string-append (assoc-ref outputs "out")
                                              "/bin/.protonvpn-real")))
               (wrap-program entrypoint
                            `("PATH" ":" prefix
                              ,(map (lambda (name)
                                      (let ((input (assoc-ref inputs name)))
                                        (string-append input "/bin:"
                                                       input "/sbin")))
                                    (list "dialog"
                                          "iproute2"
                                          "iptables"
                                          "ncurses"
                                          "openvpn"
                                          "procps"
                                          "which")))))
             #t)))))
    (native-inputs
     `(("python-docopt" ,python-docopt)))
    (inputs
     `(("dialog" ,dialog)
       ("iproute2" ,iproute)
       ("iptables" ,iptables)
       ("ncurses" ,ncurses)
       ("openvpn" ,openvpn)
       ("procps" ,procps)
       ("python-jinja2" ,python-jinja2)
       ("python-pythondialog" ,python-pythondialog)
       ("python-requests" ,python-requests)
       ("which" ,which)))
    (synopsis "Command-line client for ProtonVPN")
    (description
     "This is the official command-line interface for ProtonVPN, a secure
point-to-point virtual private networking (VPN) service with a gratis tier.
It can automatically find and connect to the fastest servers or use Tor over
VPN.  The gratis tier offers unlimited bandwidth for up to 10 devices.")
    (home-page "https://github.com/ProtonVPN/linux-cli")
    (license license:gpl3+)))

(define-public tinc
  (package
    (name "tinc")
    (version "1.0.36")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://tinc-vpn.org/packages/"
                                  "tinc-" version ".tar.gz"))
              (sha256
               (base32
                "021i2sl2mjscbm8g59d7vs74iw3gf0m48wg7w3zhwj6czarkpxs0"))))
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
       ("python-flake8" ,python-flake8)
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
             (url "https://github.com/ambrop72/badvpn")
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

(define-public wireguard-linux-compat
  (package
    (name "wireguard-linux-compat")
    (version "1.0.20201221")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.zx2c4.com/wireguard-linux-compat/"
                                  "snapshot/wireguard-linux-compat-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0ci13in0fqq32n5qamch4qhjgbdq86ygrgmfhc9szsh2nsl8jlkf"))))
    (build-system linux-module-build-system)
    (outputs '("out"
               "kernel-patch"))
    (arguments
     `(#:linux ,linux-libre-5.4         ; mustn't have WG built-in
       #:tests? #f                      ; no test suite
       #:modules ((guix build linux-module-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 textual-ports))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'change-directory
           (lambda _
             (chdir "./src")
             #t))
         (add-after 'build 'build-patch
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((patch-builder "../kernel-tree-scripts/create-patch.sh")
                    (port (open-input-pipe patch-builder))
                    (str (get-string-all port)))
               (close-pipe port)
               (call-with-output-file "wireguard.patch"
                 (lambda (port)
                   (format port "~a" str))))
             #t))
         (add-after 'install 'install-patch
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "wireguard.patch"
                           (assoc-ref %outputs "kernel-patch"))
             #t))
         ;; So that 'install-license-files' works...
         (add-before 'install-license-files 'reset-cwd
           (lambda _
             (chdir "..")
             #t)))))
    (home-page "https://git.zx2c4.com/wireguard-linux-compat/")
    (synopsis "WireGuard kernel module for Linux 3.10 through 5.5")
    (description "This package contains an out-of-tree kernel patch and
a loadable module adding WireGuard to Linux kernel versions 3.10 through 5.5.
WireGuard was added to Linux 5.6.")
    (license license:gpl2)))

(define-public wireguard-tools
  (package
    (name "wireguard-tools")
    (version "1.0.20210315")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.zx2c4.com/wireguard-tools.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1an5gm2dv111n3fylbrnyynxmi2d3iwf2b46zq08hc54kzazxcml"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target))
             "--directory=src"
             "WITH_BASHCOMPLETION=yes"
             ;; Install the ‘simple and dirty’ helper script wg-quick(8).
             "WITH_WGQUICK=yes"
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             ;; Currently used only to create an empty /etc/wireguard directory.
             (string-append "SYSCONFDIR=no-thanks"))
       ;; The test suite is meant to be run interactively.  It runs Clang's
       ;; scan-build static analyzer and then starts a web server to display the
       ;; results.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; No configure script
         (delete 'configure)
         (add-after 'install 'install-contrib-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "contrib/"
                                 (string-append out "/share/doc/wireguard-tools"))
               #t)))
         (add-after 'install 'wrap-wg-quick
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (inputs-sbin (map (lambda (input)
                                        (string-append (assoc-ref inputs input) "/sbin"))
                                      (list "resolvconf" "iproute" "procps"
                                            "iptables")))
                   (coreutils (string-append (assoc-ref inputs "coreutils")
                                             "/bin")))
               (wrap-program (string-append out "/bin/wg-quick")
                 `("PATH" ":" prefix ,(append inputs-sbin
                                              (list coreutils))))
               #t))))))
    (inputs
     `(("resolvconf" ,openresolv)
       ("coreutils" ,coreutils)
       ("bash" ,bash)                   ; for scripts using /dev/tcp
       ("procps" ,procps)
       ("iproute" ,iproute)
       ("iptables" ,iptables)))
    (home-page "https://www.wireguard.com/")
    (synopsis "Tools for configuring WireGuard tunnels")
    (description
     "This package provides the user-space command-line tools for using and
configuring WireGuard tunnels.

WireGuard is a simple and fast general-purpose @acronym{VPN, Virtual Private
Network} that securely encapsulates IP packets over UDP.  It aims to be as easy
to configure and deploy as SSH.  VPN connections are made simply by exchanging
public keys and can roam across IP addresses.")
    (license
     (list license:lgpl2.1+    ; src/netlink.h & contrib/embeddable-wg-library
           license:gpl2))))    ; everything else

(define-public wireguard
  (deprecated-package "wireguard" wireguard-tools))

(define-public xl2tpd
  (package
    (name "xl2tpd")
    (version "1.3.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xelerance/xl2tpd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0is5ccrvijz0pfm45pfrlbb9y8231yz3c4zqs8mkgakl9rxajy6l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" %output)
                          "CC=gcc")
       #:phases (modify-phases %standard-phases
                  (delete 'configure) ;no configure script
                  (add-before 'build 'setup-environment
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "l2tp.h"
                        (("/usr/sbin/pppd")
                         (string-append (assoc-ref inputs "ppp")
                                        "/sbin/pppd")))
                      (setenv "KERNELSRC"
                              (assoc-ref inputs "linux-libre-headers"))
                      #t)))
       #:tests? #f))                    ; no tests provided
    (inputs `(("libpcap" ,libpcap)
              ("linux-libre-headers" ,linux-libre-headers)
              ("ppp" ,ppp)))
    (home-page "https://www.xelerance.com/software/xl2tpd/")
    (synopsis "Layer 2 Tunnelling Protocol Daemon (RFC 2661)")
    (description
     "xl2tpd is an implementation of the Layer 2 Tunnelling Protocol (RFC 2661).
L2TP allows you to tunnel PPP over UDP.")
    (license license:gpl2)))
