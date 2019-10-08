;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Andrew Miloradovsky <andrew@interpretmath.pw>
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
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls))

(define-public keepalived
  (package
    (name "keepalived")
    (version "2.0.18")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.keepalived.org/software/keepalived-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1l2g0bzzbah9svfpwa0b9dgvwfv85r2y3qdr54822hg5p2qs48ql"))))
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
     `(("pkg-config" ,pkg-config)
       ("python-sphinx" ,python-sphinx)
       ("texinfo" ,texinfo)))
    (inputs
     `(("openssl" ,openssl)
       ("libnfnetlink" ,libnfnetlink)
       ("libnl" ,libnl)))
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
    (version "0.9.5")
    (home-page "https://github.com/canonical/raft")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q49f5mmv6nr6dxhnp044xwc6jlczgh0nj0bl6718wiqh28411x0"))))
    (arguments '(#:configure-flags '("--disable-uv")))
    ;; The uv plugin tests fail, if libuv (or the example) is enabled,
    ;; because setting up the environment requires too much privileges.
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
