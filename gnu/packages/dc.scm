;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages dc)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public ncdc
  (package
    (name "ncdc")
    (version "1.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dev.yorhel.nl/download/ncdc-" version
                           ".tar.gz"))
       (sha256 (base32
                "0n9sn4rh4zhmzjknsvyp4bfh925abz93ln43gl8a1v63rs2yyhgx"))))
    (build-system gnu-build-system)
    (inputs
     `(("bzip2" ,bzip2)
       ("glib" ,glib)
       ("gnutls" ,gnutls)
       ("ncurses" ,ncurses)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://dev.yorhel.nl/ncdc")
    (synopsis
     "Lightweight direct connect client with a friendly ncurses interface")
    (description
     "Ncdc is a client for the Direct Connect peer-to-peer protocol implemented
using ncurses.  It is known for its smaller footprint and ease of use.")
    (license license:x11)))
