;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages onc-rpc)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils))

(define-public libtirpc
  (package
    (name "libtirpc")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libtirpc/libtirpc/"
                                  version "/libtirpc-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "05zf16ilwwkzv4cccaac32nssrj3rg444n9pskiwbgk6y359an14"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-netconfig-reference
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("man/netconfig.5"
                            "man/getnetconfig.3t"
                            "man/getnetpath.3t"
                            "man/rpc.3t"
                            "src/getnetconfig.c"
                            "tirpc/netconfig.h")
               (("/etc/netconfig") (string-append (assoc-ref outputs "out")
                                                  "/etc/netconfig"))))))))
    (inputs (list mit-krb5))
    (home-page "https://sourceforge.net/projects/libtirpc/")
    (synopsis "Transport-independent Sun/ONC RPC implementation")
    (description
     "This package provides a library that implements the Sun/ONC RPC (remote
procedure calls) protocol in a transport-independent manner.  It supports both
IPv4 and IPv6.  ONC RPC is notably used by the network file system (NFS).")
    (license bsd-3)))

(define-public libtirpc/hurd
  (package/inherit libtirpc
    (name "libtirpc-hurd")
    (source (origin (inherit (package-source libtirpc))
                    (patches (search-patches "libtirpc-hurd.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libtirpc)
       ((#:configure-flags flags ''())
        ;; When cross-building the target system's krb5-config should be used.
        `(list (string-append "ac_cv_prog_KRB5_CONFIG="
                              (assoc-ref %build-inputs "mit-krb5")
                              "/bin/krb5-config")))))))

(define-public rpcbind
  (package
    (name "rpcbind")
    (version "1.2.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/" name "/" name "/"
                          version "/"
                          name "-" version ".tar.bz2"))
      (patches (search-patches "rpcbind-CVE-2017-8779.patch"))
      (sha256
       (base32 "1pp8xvprsfz8nlmmvxf829gilx0ibb08bfs3lhisxrfai5j784sn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `("--with-systemdsystemunitdir=no" "--enable-warmstarts")))
    (inputs
     (list libnsl libtirpc))
    (native-inputs
     (list pkg-config))
    (home-page "http://rpcbind.sourceforge.net/")
    (synopsis "Server to convert RPC program numbers into universal addresses")
    (description
     "@command{Rpcbind} is a server that converts RPC program numbers into
universal addresses.")
    (license bsd-3)))

(define-public rpcsvc-proto
  (package
    (name "rpcsvc-proto")
    (version "1.4")
    (home-page "https://github.com/thkukuk/rpcsvc-proto")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/releases/download/v" version
                                  "/rpcsvc-proto-" version ".tar.xz"))
              (sha256
               (base32
                "0i93wbpw5dk2gf5v4a5hq6frh814wzgjydh7saj28wlgbpqdaja1"))))
    (build-system gnu-build-system)
    (synopsis "RPCSVC protocol definitions")
    (description
     "This package provides @code{rpcsvc} @file{protocol.x} files and headers
that are not included with the @code{libtirpc} package.  Additionally it
contains @command{rpcgen}, which is used to produce header files and sources
from the protocol files.")
    (license bsd-3)))

(define-public libnsl
  (package
    (name "libnsl")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/thkukuk/libnsl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dayj5i4bh65gn7zkciacnwv2a0ghm6nn58d78rsi4zby4lyj5w5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list libtirpc))
    (synopsis "Public client interface for NIS(YP) and NIS+")
    (description "Libnsl is the public client interface for the Network
Information Service / Yellow Pages (NIS/YP) and NIS+.  It includes IPv6 support.
This library was part of glibc < 2.26, but is now distributed separately.")
    (home-page "https://github.com/thkukuk/libnsl")
    ;; The package is distributed under the LGPL 2.1. Some files in
    ;; 'src/nisplus/' are LGPL 2.1+, and some files in 'src/rpcsvc/' are BSD-3.
    (license lgpl2.1)))
