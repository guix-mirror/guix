;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
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
    (version "2.76")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.thekelleys.org.uk/dnsmasq/dnsmasq-"
                    version ".tar.xz"))
              (sha256
               (base32
                "15lzih6671gh9knzpl8mxchiml7z5lfqzr7jm2r0rjhrxs6nk4jb"))))
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

(define-public bind
  (package
    (name "bind")
    (version "9.10.4-P2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.isc.org/isc/bind9/" version "/" name "-"
                    version ".tar.gz"))
              (sha256
               (base32
                "08s48h5p916ixjiwgar4w6skc20crmg7yj1y7g89c083zvw8lnxk"))))
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
    (description "BIND is an implementation of the Domain Name System (DNS)
protocols for the Internet.  It is a reference implementation of those
protocols, but it is also production-grade software, suitable for use in
high-volume and high-reliability applications. The name BIND stands for
\"Berkeley Internet Name Domain\", because the software originated in the early
1980s at the University of California at Berkeley.")
    (home-page "https://www.isc.org/downloads/bind")
    (license (list license:isc))))

