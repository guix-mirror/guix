;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages databases)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public dnsmasq
  (package
    (name "dnsmasq")
    (version "2.72")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.thekelleys.org.uk/dnsmasq/dnsmasq-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1c80hq09hfm8cp5pirfb8wdlc7dqkp7zzmbmdaradcvlblzx42vx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-delete 'configure %standard-phases)
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")
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

(define-public bind-utils
  (package
    (name "bind-utils")
    (version "9.10.3-P4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.isc.org/isc/bind9/" version
                                  "/bind-" version ".tar.gz"))
              (sha256
               (base32
                "0giys46ifypysf799w9v58kbaz1v3fbdzw3s212znifzzfsl9h1a"))))
    (build-system gnu-build-system)
    (inputs
     ;; it would be nice to add GeoIP and gssapi once there is package
     `(("libcap" ,libcap)
       ("libxml2" ,libxml2)
       ("mysql" ,mysql)
       ("openssl" ,openssl)
       ("perl" ,perl)
       ("p11-kit" ,p11-kit)))
    (arguments
     `(#:tests? #f ; no test phase implemented
       #:configure-flags
       (list (string-append "--with-openssl="
                            (assoc-ref %build-inputs "openssl"))
             (string-append "--with-dlz-mysql="
                            (assoc-ref %build-inputs "mysql"))
             (string-append "--with-pkcs11="
                            (assoc-ref %build-inputs "p11-kit")))
       #:modules ((srfi srfi-1)
                  (srfi srfi-26)
                  ,@%gnu-build-system-modules)
       #:phases
       (let ((libs '("dns" "isc" "bind9" "isccfg" "lwres"))
             (bins '("dig" "nsupdate")))
         (modify-phases %standard-phases
           (replace 'build
             (lambda _
               (every (lambda (dir)
                        (zero? (system* "make" "-C" dir)))
                      (append (map (cut string-append "lib/" <>) libs)
                              (map (cut string-append "bin/" <>) bins)))))
           (replace 'install
             (lambda _
               (every (lambda (dir)
                        (zero? (system* "make" "-C" dir "install")))
                      (map (cut string-append "bin/" <>) bins))))))))
    (home-page "https://www.isc.org/downloads/bind/")
    (synopsis "Tools for querying nameservers")
    (description
     "These tools, included with ISC BIND, are useful for analysis of DNS
issues or verification of configuration.")
    (license (list license:isc))))
