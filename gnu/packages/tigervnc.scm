;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Todor Kondić <tk.code@protonmail.com>
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

(define-module (gnu packages tigervnc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (guix build-system cmake)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public tigervnc-client
  (package
    (name "tigervnc-client")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/TigerVNC/tigervnc.git")
         (commit (string-append "v" version))))
       (sha256
        (base32
         "0b47fg3741qs3zdpl2zr0s6jz46dypp2j6gqrappbzm3ywnnmm1x"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; Tests that do exists are not automated.
       #:phases (modify-phases %standard-phases
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (with-directory-excursion "vncviewer"
                        (invoke "make" "install")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("gettext-minimal" ,gettext-minimal)
       ("automake" ,automake)))
    (inputs
     `(("zlib" ,zlib)
       ("gnutls" ,gnutls)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("fltk" ,fltk)
       ("linux-pam" ,linux-pam)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxtst" ,libxtst)
       ("libxrandr" ,libxrandr)
       ("libxdamage" ,libxdamage)))
    (home-page "https://tigervnc.org/")
    (synopsis "High-performance, platform-neutral
implementation of VNC (client)")
    (description "TigerVNC is a client/server implementation of VNC (Virtual
Network Computing).  It provides enough performance to run even 3D and video
applications.  It also provides extensions for advanced authentication methods
and TLS encryption.  This package installs only the VNC client, the
application which is needed to connect to VNC servers.")
    (license license:gpl2)))
