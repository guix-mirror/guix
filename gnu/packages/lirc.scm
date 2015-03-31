;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
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

(define-module (gnu packages lirc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages python))

(define-public lirc
  (package
    (name "lirc")
    (version "0.9.2a")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lirc/lirc-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "011nwpxm5d12rsapljg3pjf9pgb0j8ngmc3zg69q4kv61hkx2zim"))
              (patches (list (search-patch "lirc-localstatedir.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--localstatedir=/var")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11)
       ("alsa-lib" ,alsa-lib)
       ("python" ,python)))
    (home-page "http://www.lirc.org/")
    (synopsis "Linux Infrared Remote Control")
    (description
     "LIRC allows computers to send and receive IR signals of many commonly
used remote controls.  The most important part of LIRC is the 'lircd' daemon
that decodes IR signals received by the device drivers.  The second daemon
program 'lircmd' allows to translate IR signals to mouse movements.  The
user space applications allow you to control your computer with a remote
control: you can send X events to applications, start programs and much more
on just one button press.")
    (license license:gpl2+)))
