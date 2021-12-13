;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Andrew Miloradovsky <andrew@interpretmath.pw>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Dion Mendel <guix@dm9.info>
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

(define-module (gnu packages cluster)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls))

(define-public drbd-utils
  (package
    (name "drbd-utils")
    (version "9.19.1")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://pkg.linbit.com/downloads/drbd"
                                        "/utils/drbd-utils-" version ".tar.gz")))
              (sha256
               (base32
                "1l99kcrb0j85wxxmrdihpx9bk1a4sdi7wlp5m1x5l24k8ck1m5cf"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "scripts/global_common.conf"
                    ;; Do not participate in usage count survey by default.
                    (("usage-count yes")
                     "usage-count no"))
                  (substitute* "scripts/Makefile.in"
                    ;; Install the Pacemaker resource agents to the libdir,
                    ;; regardless of what the OCF specification says...
                    (("\\$\\(DESTDIR\\)/usr/lib")
                     "$(DESTDIR)$(LIBDIR)"))
                  (substitute* "configure"
                    ;; Use a sensible default udev rules directory.
                    (("default_udevdir=/lib/udev")
                     "default_udevdir='${prefix}/lib/udev'"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '(;; Do not install sysv or systemd init scripts.
                           "--with-initscripttype=none"
                           ;; Use the pre-built manual pages present in release
                           ;; tarballs instead of generating them from scratch.
                           "--with-prebuiltman"
                           ;; Disable support for DRBD 8.3 as it is only for
                           ;; Linux-Libre versions < 3.8.  8.4 is the latest
                           ;; kernel driver as of Linux 5.7.
                           "--without-83support"
                           "--sysconfdir=/etc"
                           "--localstatedir=/var")
       #:test-target "test"
       #:make-flags '("WANT_DRBD_REPRODUCIBLE_BUILD=yesplease")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-generated-file-shebangs 'patch-documentation
           (lambda _
             ;; The preceding phase misses some Makefiles with unusual file
             ;; names, so we handle those here.
             (for-each patch-makefile-SHELL (find-files "documentation/common"
                                                        "^Makefile"))
             #t))
         (add-before 'configure 'use-absolute-/lib/drbd
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Look for auxiliary executables below exec_prefix instead
               ;; of assuming /lib/drbd (see TODO comment in the file).
               (substitute* "user/v9/drbdtool_common.c"
                 (("\"/lib/drbd\"")
                  (string-append "\"" out "/lib/drbd\"")))
               #t)))
         (add-after 'configure 'adjust-installation-directories
           (lambda _
             ;; Do not attempt to create /etc or /var.
             (substitute* "scripts/Makefile"
               (("\\$\\(DESTDIR\\)\\$\\(sysconfdir\\)")
                "$(DESTDIR)$(prefix)$(sysconfdir)"))
             (substitute* "user/v84/Makefile"
               (("\\$\\(DESTDIR\\)\\$\\(localstatedir\\)")
                "$(DESTDIR)$(prefix)$(localstatedir)")
               (("\\$\\(DESTDIR\\)/lib/drbd")
                "$(DESTDIR)$(prefix)/lib/drbd"))
             (substitute* "user/v9/Makefile"
               (("\\$\\(DESTDIR\\)\\$\\(localstatedir\\)")
                "$(DESTDIR)$(prefix)$(localstatedir)")
               (("\\$\\(DESTDIR\\)\\$\\(DRBD_LIB_DIR\\)")
                "$(DESTDIR)$(prefix)$(DRBD_LIB_DIR)"))
             #t)))))
    (native-inputs
     `(("clitest" ,clitest)
       ("flex" ,flex)
       ("udev" ,eudev)))          ;just to satisfy a configure check
    (home-page "https://www.linbit.com/drbd/")
    (synopsis "Replicate block devices between machines")
    (description
     "@acronym{DRBD, Distributed Replicated Block Device} is a software-based,
shared-nothing, replicated storage solution mirroring the content of block
devices (hard disks, partitions, logical volumes etc.) over any network
connection.  This package contains the userland utilities.")
    (license license:gpl2+)))

(define-public keepalived
  (package
    (name "keepalived")
    (version "2.0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.keepalived.org/software/keepalived-"
                    version ".tar.gz"))
              (sha256
               (base32
                "19scrrjsxw5g914d5ka352445blaq77dk2vm4vxabijvfra88bqf"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-info
           (lambda _
             (invoke "make" "-C" "doc" "texinfo")
             ;; Put images in a subdirectory as recommended by 'texinfo'.
             (install-file "doc/source/images/software_design.png"
                           "doc/build/texinfo/keepalived-figures")
             (substitute* "doc/build/texinfo/keepalived.texi"
               (("@image\\{software_design,")
                "@image{keepalived-figures/software_design,"))
             (invoke "make" "-C" "doc/build/texinfo")))
         (add-after 'install 'install-info
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (infodir (string-append out "/share/info")))
               (install-file "doc/build/texinfo/keepalived.info" infodir)
               (install-file "doc/source/images/software_design.png"
                             (string-append infodir "/keepalived-figures"))
               #t))))))
    (native-inputs
     (list pkg-config python-sphinx texinfo))
    (inputs
     (list openssl libnfnetlink libnl))
    (home-page "https://www.keepalived.org/")
    (synopsis "Load balancing and high-availability frameworks")
    (description
     "Keepalived provides frameworks for both load balancing and high
availability.  The load balancing framework relies on the Linux Virtual
Server (@dfn{IPVS}) kernel module.  High availability is achieved by the Virtual
Redundancy Routing Protocol (@dfn{VRRP}).  Each Keepalived framework can be used
independently or together to provide resilient infrastructures.")
    (license license:gpl2+)))

(define-public libraft
  (package
    (name "libraft")
    (version "0.11.2")
    (home-page "https://github.com/canonical/raft")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "050dwy34jh8dihfwfm0r1by2i3sy9crapipp9idw32idm79y4izb"))))
    (arguments '(#:configure-flags '("--enable-uv")
                 #:phases
                 (modify-phases %standard-phases
                   (add-after 'unpack 'disable-failing-tests
                     (lambda _
                       (substitute* "Makefile.am"
                         ((".*test_uv_append.c.*") ""))
                       #t)))))
    (inputs
     (list libuv lz4))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (build-system gnu-build-system)
    (synopsis "C implementation of the Raft consensus protocol")
    (description "The library has modular design: its core part implements only
the core Raft algorithm logic, in a fully platform independent way.  On top of
that, a pluggable interface defines the I/O implementation for networking
(send/receive RPC messages) and disk persistence (store log entries and
snapshots).")
    (license license:asl2.0)))

(define-public libdqlite
  (package
    (name "libdqlite")
    (version "1.9.0")
    (home-page "https://github.com/canonical/dqlite")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zalsvr0vy7632nhm96a29lrfy18iqsmbxpyz2lvq80mrjlbrzsn"))))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "Makefile.am"
               ;; Test client/query sometimes fails.
               ;; The actual tested asserts succeed, but there appears to be a
               ;; race condition when tearing down the test server.
               ((".*test_client.c.*") "")))))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list libraft libuv))
    (propagated-inputs
     (list sqlite))  ; dqlite.h includes sqlite3.h
    (build-system gnu-build-system)
    (synopsis "Distributed SQLite")
    (description "dqlite is a C library that implements an embeddable and replicated
SQL database engine with high-availability and automatic failover.")
    (license license:lgpl3)))
