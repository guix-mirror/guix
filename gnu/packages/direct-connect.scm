;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages direct-connect)
  #:use-module (guix build-system scons)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

(define-public linuxdcpp
  (package
    (name "linuxdcpp")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://launchpad.net/linuxdcpp/1.1/1.1.0/+download/linuxdcpp-"
             version ".tar.bz2"))
       (sha256
        (base32
         "12i92hirmwryl1qy0n3jfrpziwzb82f61xca9jcjwyilx502f0b6"))))
    (build-system scons-build-system)
    (arguments
     `(#:scons ,scons-python2
       #:scons-flags (list (string-append "PREFIX=" %output))
       #:tests? #f)) ; no tests
    (inputs
     `(("boost" ,boost)
       ("bzip2" ,bzip2)
       ("gtk+" ,gtk+-2)
       ("libglade" ,libglade)
       ("libnotify" ,libnotify)
       ("openssl" ,openssl)))
    (native-inputs
     `(("bazaar" ,bazaar)
       ("gettext-minimal" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (home-page "https://launchpad.net/linuxdcpp/")
    (synopsis "Direct Connect client")
    (description "LinuxDC++ is a Direct Connect (DC) client.  Direct Connect
is a peer-to-peer file-sharing protocol.  Clients connect to a central hub
where they can chat or share files with one another.  Users can view other
users' list of shared files or search the hub for files.")
    (license license:gpl2+)))
