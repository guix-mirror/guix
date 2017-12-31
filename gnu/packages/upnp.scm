;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages upnp)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public miniupnpc
  (package
    (name "miniupnpc")
    (version "2.0.20171212")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://miniupnp.tuxfamily.org/files/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "0za7pr6hrr3ajkifirhhxfn3hlhl06f622g8hnj5h8y18sp3bwff"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)))
    (arguments
     ;; The build system does not use a configure script but depends on
     ;; `make'.  Hence we should pass parameters to `make' instead and remove
     ;; the configure phase.
     '(#:make-flags
       (list
        (string-append "SH=" (assoc-ref %build-inputs "bash") "/bin/sh")
        (string-append "INSTALLPREFIX=" (assoc-ref %outputs "out"))
        "CC=gcc"

        ;; Allow executables to find libminiupnpc.so.
        (string-append "LDFLAGS=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'qualify-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "external-ip.sh"
               (("upnpc")
                (string-append (assoc-ref outputs "out") "/bin/upnpc")))
             #t)))))
    (home-page "http://miniupnp.free.fr/")
    (synopsis "UPnP protocol client library")
    (description
     "The MiniUPnPc client library facilitates access to the services provided
by any @dfn{Universal Plug and Play} (UPnP) @dfn{Internet Gateway Device} (IGD)
present on the network.  In UPnP terminology, MiniUPnPc is a UPnP Control Point.

It is useful whenever an application needs to listen for incoming connections
while running behind a UPnP-enabled router or firewall.  Such applications
include peer-to-peer applications, active-mode FTP clients, DCC file transfers
over IRC, instant messaging, network games, and most server software.")
    (license
     (x11-style "file://LICENSE" "See 'LICENSE' file in the distribution"))))

(define-public libupnp
  (package
    (name "libupnp")
    (version "1.6.24")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/pupnp/pupnp/libUPnP%20"
                          version "/" name "-" version ".tar.bz2"))
      (sha256
       (base32
        "15ngi1i7cvsv7g15fb9bkswvi99d1plz52x5qgjn4h5vyfddg0vx"))))
    (build-system gnu-build-system)
    (arguments
     ;; The tests require a network device capable of multicasting which is
     ;; not available in the build environment. See
     ;; https://lists.gnu.org/archive/html/guix-devel/2015-01/msg00312.html.
     `(#:tests? #f
       #:configure-flags '("--enable-ipv6")))
    (home-page "http://pupnp.sourceforge.net")
    (synopsis "Portable SDK for UPnP Devices")
    (description
     "The portable SDK for UPnP Devices (libupnp) provides developers with an
API and code for building control points, devices, and bridges that are
compliant with Version 1.0 of the Universal Plug and Play Device Architecture
Specification and support several operating systems like Linux, *BSD, Solaris
and others.")
    (license bsd-3)))
