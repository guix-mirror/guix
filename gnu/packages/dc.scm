;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
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
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages databases)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses)
  #:renamer (symbol-prefix-proc 'license:)))

(define-public ncdc
  (package
    (name "ncdc")
    (version "1.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://dev.yorhel.nl/download/ncdc-" version
                           ".tar.gz"))
       (sha256 (base32
                "1wgvqwfxq9kc729h2r528n55821w87sfbm4h21mr6pvkpfw30hf2"))))
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
    (home-page "http://dev.yorhel.nl/ncdc")
    (synopsis
     "Lightweight direct connect client with a friendly ncurses interface")
    (description
     "Ncdc is a client for the Direct Connect peer-to-peer protocol implemented
using ncurses.  It is known for its smaller footprint and ease of use.")
    (license license:x11)))
