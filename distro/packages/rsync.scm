;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Andreas Enge <andreas@enge.fr>
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

(define-module (distro packages rsync)
  #:use-module (distro)
  #:use-module (distro packages perl)
  #:use-module (distro packages acl)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))


(define-public rsync
  (package
   (name "rsync")
   (version "3.0.9")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://rsync.samba.org/ftp/rsync/rsync-"
                                version ".tar.gz"))
            (sha256
             (base32
              "01bw4klqsrlhh3i9lazd485sd9qx5djvnwa21lj2h3a9sn6hzw9h"))))
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
