;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages tor)
  #:use-module ((guix licenses) #:select (bsd-3 gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages w3m))

(define-public tor
  (package
    (name "tor")
    (version "0.2.4.21")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://www.torproject.org/dist/tor-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1kpijqapml7y4sl54qgyrzppxxhmy4xgk2y7wkqwjxn7q24g97d1"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("openssl" ,openssl)
       ("libevent" ,libevent)))

    ;; TODO: Recommend `torsocks' since `torify' needs it.

    (home-page "http://www.torproject.org/")
    (synopsis "An anonymous network router to improve privacy on the Internet")
    (description
     "Tor protects you by bouncing your communications around a distributed
network of relays run by volunteers all around the world: it prevents
somebody watching your Internet connection from learning what sites you
visit, and it prevents the sites you visit from learning your physical
location. Tor works with many of your existing applications, including
web browsers, instant messaging clients, remote login, and other
applications based on the TCP protocol.")
    (license bsd-3)))

(define-public torsocks
  (package
    (name "torsocks")
    (version "1.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://torsocks.googlecode.com/files/torsocks-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1m0is5q24sf7jjlkl0icfkdc0m53nbkg0q72s57p48yp4hv7v9dy"))))
    (build-system gnu-build-system)
    (home-page "http://code.google.com/p/torsocks/")
    (synopsis "Use socks-friendly applications with Tor")
    (description
     "Torsocks allows you to use most socks-friendly applications in a safe
way with Tor.  It ensures that DNS requests are handled safely and explicitly
rejects UDP traffic from the application you're using.")
    (license gpl2+)))

(define-public privoxy
  (package
    (name "privoxy")
    (version "3.0.21")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/ijbswa/Sources/"
                                 version "%20%28stable%29/privoxy-"
                                 version "-stable-src.tar.gz"))
             (sha256
              (base32
               "1f6xb7aa47p90c26vqaw74y6drs9gpnhxsgby3mx0awdjh0ydisy"))))
    (build-system gnu-build-system)
    (arguments
     '(;; The default 'sysconfdir' is $out/etc; change that to
       ;; $out/etc/privoxy.
       #:configure-flags (list (string-append "--sysconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc/privoxy"))
       #:phases (alist-cons-before
                 'configure 'autoconf
                 (lambda _
                   ;; Unfortunately, this is not a tarball produced by
                   ;; "make dist".
                   (zero? (system* "autoreconf" "-vfi")))
                 %standard-phases)
       #:tests? #f))
    (inputs
     `(("w3m" ,w3m)
       ("pcre" ,pcre)
       ("zlib" ,zlib)
       ("autoconf" ,autoconf)
       ("automake" ,automake)))
    (home-page "http://www.privoxy.org")
    (synopsis "Web proxy with advanced filtering capabilities for enhancing privacy")
    (description
     "Privoxy is a non-caching web proxy with advanced filtering capabilities
for enhancing privacy, modifying web page data and HTTP headers, controlling
access, and removing ads and other obnoxious Internet junk.  Privoxy has a
flexible configuration and can be customized to suit individual needs and
tastes.  It has application for both stand-alone systems and multi-user
networks.")
    (license gpl2+)))
