;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages rsync)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages which)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))


(define-public rsync
  (package
   (name "rsync")
   (version "3.1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://rsync.samba.org/ftp/rsync/src/rsync-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0kirw8wglqvwi1v8bwxp373g03xg857h59j5k3mmgff9gzvj7jl1"))))
   (build-system gnu-build-system)
   (inputs `(("perl" ,perl)
             ("acl" ,acl)))
   (synopsis "rsync, a remote (and local) file copying tool")
   (description
    "rsync is a fast and versatile file copying tool. It can copy locally,
to/from another host over any remote shell, or to/from a remote rsync daemon.
Its delta-transfer algorithm reduces the amount of data sent over the network
by sending only the differences between the source files and the existing
files in the destination.")
   (license gpl3+)
   (home-page "http://rsync.samba.org/")))

(define-public librsync
  (package
    (name "librsync")
    (version "0.9.7")
       (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/projects/librsync/librsync/"
                                version "/librsync-" version ".tar.gz"))
            (sha256
             (base32
              "1mj1pj99mgf1a59q9f2mxjli2fzxpnf55233pc1klxk2arhf8cv6"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("which" ,which)
      ("perl" ,perl)))
   (arguments '(#:configure-flags '("--enable-shared")))
   (home-page "http://librsync.sourceforge.net/")
   (synopsis "Implementation of the rsync remote-delta algorithm")
   (description
    "Librsync is a free software library that implements the rsync
remote-delta algorithm.  This algorithm allows efficient remote updates of a
file, without requiring the old and new versions to both be present at the
sending end. The library uses a \"streaming\" design similar to that of zlib
with the aim of allowing it to be embedded into many different applications.")
   (license lgpl2.1+)))
