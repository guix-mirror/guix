;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Leo Famulari <leo@famulari.name>
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
  #:use-module (gnu packages perl)
  #:use-module (gnu packages popt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu))


(define-public rsync
  (package
   (name "rsync")
   (version "3.1.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://rsync.samba.org/ftp/rsync/src/rsync-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1h0011dj6jgqpgribir4anljjv7bbrdcs8g91pbsmzf5zr75bk2m"))))
   (build-system gnu-build-system)
   (inputs `(("perl" ,perl)
             ("acl" ,acl)))
   (synopsis "Remote (and local) file copying tool")
   (description
    "Rsync is a fast and versatile file copying tool.  It can copy locally,
to/from another host over any remote shell, or to/from a remote rsync daemon.
Its delta-transfer algorithm reduces the amount of data sent over the network
by sending only the differences between the source files and the existing
files in the destination.")
   (license license:gpl3+)
   (home-page "http://rsync.samba.org/")))

(define-public librsync
  (package
    (name "librsync")
    (version "2.0.2")
       (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/librsync/librsync/archive/v"
                                version ".tar.gz"))
            (sha256
             (base32
              "1waa581pcscc1rnvy06cj584k5dx0dc7jj79wsdj7xw4xqh9ayz6"))))
   (build-system cmake-build-system)
   (inputs
    `(("popt" ,popt)))
   (native-inputs
    `(("which" ,which)
      ("perl" ,perl)))
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
