;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
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
  #:use-module (gnu packages acl)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages popt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu))

(define-public rsync
  (package
   (name "rsync")
   (version "3.2.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://rsync.samba.org/ftp/rsync/src/rsync-"
                                version ".tar.gz"))
            (sha256
             (base32
              "03p5dha9g9krq61mdbcrjkpz5nglri0009ks2vs9k97f9i83rk5y"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags
      ;; The bundled copies are preferred by default.
      (list "--without-included-zlib"
            "--without-included-popt"
            ;; Avoid these dependencies for now.
            "--disable-openssl")))
   (native-inputs
    (list perl))
   (inputs
    (list acl
          lz4
          popt
          xxhash
          zlib
          `(,zstd "lib")))
   (synopsis "Remote (and local) file copying tool")
   (description
    "Rsync is a fast and versatile file copying tool.  It can copy locally,
to/from another host over any remote shell, or to/from a remote rsync daemon.
Its delta-transfer algorithm reduces the amount of data sent over the network
by sending only the differences between the source files and the existing
files in the destination.")
   (license license:gpl3+)
   (home-page "https://rsync.samba.org/")))

(define-public librsync
  (package
    (name "librsync")
    (version "2.3.2")
       (source (origin
            (method git-fetch)
            (uri (git-reference
                   (url "https://github.com/librsync/librsync")
                   (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0bn29npmbw26akc6y54661irpdh6qcivcs6q48cgp3llklhhxp0q"))))
   (build-system cmake-build-system)
   (inputs
    (list popt))
   (native-inputs
    (list which perl))
   (home-page "http://librsync.sourceforge.net/")
   (synopsis "Implementation of the rsync remote-delta algorithm")
   (description
    "Librsync is a free software library that implements the rsync
remote-delta algorithm.  This algorithm allows efficient remote updates of a
file, without requiring the old and new versions to both be present at the
sending end.  The library uses a \"streaming\" design similar to that of zlib
with the aim of allowing it to be embedded into many different applications.")
   (license license:lgpl2.1+)))

(define-public librsync-0.9
  (package
    (inherit librsync)
    (version "0.9.7")
        (source (origin
             (method url-fetch)
            (uri (string-append "mirror://sourceforge/librsync/librsync/"
                                version "/librsync-" version ".tar.gz"))
             (sha256
              (base32
              "1mj1pj99mgf1a59q9f2mxjli2fzxpnf55233pc1klxk2arhf8cv6"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-shared")))
    (inputs '())))
