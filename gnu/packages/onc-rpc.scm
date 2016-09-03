;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages onc-rpc)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages mit-krb5)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu))

(define-public libtirpc
  (package
    (name "libtirpc")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libtirpc/libtirpc/"
                                  version "/libtirpc-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "18a337wa4amf0k21wnimp3yzs5l3cxqndz4x3x8bm993zhfy5hs5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remote-dangling-symlink
           (lambda _
             ;; Remove the dangling symlinks since it breaks the
             ;; 'patch-source-shebangs' file tree traversal.
             (delete-file "INSTALL"))))))
    (inputs `(("mit-krb5" ,mit-krb5)))
    (home-page "http://sourceforge.net/projects/libtirpc/")
    (synopsis "Transport-independent Sun/ONC RPC implementation")
    (description
     "This package provides a library that implements the Sun/ONC RPC (remote
procedure calls) protocol in a transport-independent manner.  It supports both
IPv4 and IPv6.  ONC RPC is notably used by the network file system (NFS).")
    (license bsd-3)))

(define-public rpcbind
  (package
    (name "rpcbind")
    (version "0.2.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/" name "/" name "/"
                          version "/"
                          name "-" version ".tar.bz2"))
      (sha256
       (base32
        "0yyjzv4161rqxrgjcijkrawnk55rb96ha0pav48s03l2klx855wq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `("--with-systemdsystemunitdir=no" "--enable-warmstarts")))
    (inputs
     `(("libtirpc" ,libtirpc)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://rpcbind.sourceforge.net/")
    (synopsis "Server to convert RPC program numbers into universal addresses")
    (description
     "@command{Rpcbind} is a server that converts RPC program numbers into
universal addresses.")
    (license bsd-3)))

