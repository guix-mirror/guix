;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages network-manager)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages tls))

(define-public network-manager
  (package
    (name "network-manager")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/NetworkManager/"
                                  (version-major+minor version)
                                  "/NetworkManager-" version ".tar.xz"))
              (sha256
               (base32
                "1j8qw3759gzckbvhnl7shm888q09q8zd1zfr19fxkf7lyji9y2g4"))
              ;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
              (patches
               (list (search-patch "network-manager-platform-managed.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("glib:bin" ,glib "bin") ; for gdbus-codegen, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("python" ,python-wrapper)
       ("python-pygobject" ,python-pygobject)  ; for tests
       ("python-dbus" ,python-dbus)            ; for tests
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("dbus-glib" ,dbus-glib)
       ("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)))
    (inputs
     `(("wireless-tools" ,wireless-tools)
       ;; XXX Investigate:
       ;;   checking for LIBNL... yes
       ;;   checking for rtnl_link_inet6_get_addr_gen_mode in -lnl-route-3... no
       ;;   checking Linux kernel IN6_ADDR_GEN_MODE enum... no
       ;;   checking for rtnl_link_inet6_get_token in -lnl-route-3... no
       ("libnl" ,libnl)
       ("ppp" ,ppp)
       ("libndp" ,libndp)
       ("newt" ,newt)
       ;; TODO: libmm, libteam, resolvconf, pppoe, open-iscsi
       ("bash-host" ,bash)
       ("sed-host" ,sed)
       ("coreutils-host" ,coreutils)
       ("kmod" ,kmod)
       ("tzdata" ,tzdata)
       ("iproute" ,iproute)
       ("inetutils" ,inetutils)
       ("ethtool" ,ethtool)
       ("dnsmasq" ,dnsmasq)
       ("polkit" ,polkit)
       ("libuuid" ,util-linux)
       ("libgudev" ,libgudev)   ; XXX should we use eudev instead?
       ("libsoup" ,libsoup)
       ("iptables" ,iptables)
       ("isc-dhcp" ,isc-dhcp)
       ("bluez" ,bluez)
       ("readline" ,readline)))
    ;; XXX FIXME Investigate:
    ;; [nm-session-monitor-ck.c:290] nm_session_monitor_init(): Error loading /var/run/ConsoleKit/database: Error statting file /var/run/ConsoleKit/database: No such file or directory
    (arguments
     `(#:configure-flags
       (list "--with-distro=exherbo"
             "--with-crypto=gnutls"
             "--with-libsoup=yes"
             "--sysconfdir=/etc"
             "--localstatedir=/var"
             "--with-kernel-firmware-dir=/run/current-system/kernel/lib/firmware"
             (string-append "--with-dbus-sys-dir="
                            (assoc-ref %outputs "out")
                            "/etc/dbus-1/system.d")
             (string-append "--with-udev-dir="
                            (assoc-ref %outputs "out")
                            "/lib/udev")
             (string-append "--with-dhclient="
                            (assoc-ref %build-inputs "isc-dhcp")
                            "/sbin/dhclient")
             (string-append "--with-iptables="
                            (assoc-ref %build-inputs "iptables")
                            "/bin/iptables")
             (string-append "--with-dnsmasq="
                            (assoc-ref %build-inputs "dnsmasq")
                            "/sbin/dnsmasq")
             #;
             (string-append "--with-resolvconf="
                            (assoc-ref %build-inputs "resolvconf")
                            "/sbin/resolvconf")
             (string-append "--with-pppd="
                            (assoc-ref %build-inputs "ppp")
                            "/bin/pppd"))
       #:strip-binaries? #f   ; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-file-names
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "data/85-nm-unmanaged.rules"
              (("/bin/sh")
               (string-append (assoc-ref inputs "bash-host") "/bin/sh"))
              (("/bin/sed")
               (string-append (assoc-ref inputs "sed-host") "/bin/sed"))
              (("/bin/cat")
               (string-append (assoc-ref inputs "coreutils-host") "/bin/cat"))
              (("/usr/sbin/ethtool")
               (string-append (assoc-ref inputs "ethtool") "/sbin/ethtool")))
            (substitute* "src/NetworkManagerUtils.c"
              (("/sbin/modprobe")
               (string-append (assoc-ref inputs "kmod") "/bin/modprobe")))
            (substitute* "src/dhcp-manager/systemd-dhcp/src/shared/util.c"
              (("/bin/sh")
               (string-append (assoc-ref inputs "bash-host") "/bin/sh")))
            (substitute* "src/dhcp-manager/systemd-dhcp/src/shared/time-util.c"
              (("/usr/share/zoneinfo")
               (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo")))
            (substitute* "src/devices/nm-device.c"
              (("/usr/bin/ping") "/run/setuid-programs/ping"))
            ;; XXX TODO:
            ;;
            ;; ./src/dns-manager/nm-dns-unbound.c:45:
            ;;   return nm_spawn_process ("/usr/libexec/dnssec-trigger-script --async --update", NULL) == 0;
            ;;
            ;; ./src/NetworkManagerUtils.c:1062:
            ;;   const char *const NM_PATHS_DEFAULT[] = {
            ;;     PREFIX "/sbin/",
            ;;     PREFIX "/bin/",
            ;;     "/sbin/",
            ;;     "/usr/sbin/",
            ;;     "/usr/local/sbin/",
            ;;     "/bin/",
            ;;     "/usr/bin/",
            ;;     "/usr/local/bin/",
            ;;     NULL,
            ;;   };
            ;;
            ;; The following substitution will be needed if we add open-iscsi
            ;; support.
            #;
            (substitute* "src/settings/plugins/ibft/plugin.c"
              (("/sbin/iscsiadm")
               (string-append (assoc-ref inputs "open-iscsi")
                              "/sbin/iscsiadm")))
            #t))
         (add-after
          'unpack 'fix-tests
          (lambda _
            (substitute* (find-files "src/settings/plugins/ibft/tests"
                                     "^iscsiadm-test-")
              (("^cat") (which "cat")))
            (substitute* "src/settings/plugins/ifupdown/tests/test-ifupdown.c"
              (("/sbin/ifconfig") (which "ifconfig")))
            (substitute* "src/platform/tests/test-link.c"
              (("/sbin/ip") (which "ip")))
            (substitute* "src/platform/tests/test-common.c"
              (("/usr/sbin") (dirname (which "ip")))
              ;; XXX FIXME Fix tests that use linux containers.  For now, we
              ;; disable unsharing, which causes those tests to be skipped.
              (("!unshare_user \\(\\)") "1"))
            #t))
         (replace
          'install
          (lambda* (#:key outputs (make-flags '()) #:allow-other-keys)
            ;; Override sysconfdir and localstatedir during "make install", to
            ;; avoid attempting to install in /etc and /var, and to instead
            ;; install the skeletons in the output directory.
            ;; XXX FIXME: consider setting these instead to /tmp/{etc,var}.
            (let ((out (assoc-ref outputs "out")))
             (zero? (apply system*
                           "make" "install"
                           (string-append "sysconfdir=" out "/etc")
                           (string-append "localstatedir=" out "/var")
                           make-flags))))))))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager")
    (synopsis "Network management framework")
    (description
     "NetworkManager is a system network service that manages your network
devices and connections, attempting to keep active network connectivity when
available.  It manages ethernet, WiFi, mobile broadband (WWAN), and PPPoE
devices, and provides VPN integration with a variety of different VPN
services.")
    ;; Most files are under gpl2+, but libnm-util/* and libnm-glib/* are under
    ;; lgpl2.0+.
    (license (list license:gpl2+ license:lgpl2.0+))))
