;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages rdesktop)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml))

(define-public rdesktop
  (package
    (name "rdesktop")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rdesktop/rdesktop/"
                                  "releases/download/v" version "/rdesktop-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1222f2srlq16bydhy44gph997iajg39sl774xxh9jdwi4cqjyg27"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list ;; XXX: optional dependencies missing
                               "--disable-credssp"
                               "--disable-smartcard")
       #:tests? #f))                    ; No 'check' target
    (native-inputs
     (list pkg-config))
    (inputs
     (list gnutls libx11 libxcursor nettle))
    (home-page "https://www.rdesktop.org/")
    (synopsis "Client for Windows Terminal Services")
    (description
     "rdesktop is a client for Microsoft's Windows Remote Desktop Services,
capable of natively speaking Remote Desktop Protocol (RDP).  It allows users
to remotely control a user's Windows desktop.")
    (license license:gpl3+)))

(define-public freerdp
  (package
    (name "freerdp")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://github.com/FreeRDP/FreeRDP")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02zlg5r704zbryx09a5rjjf7q137kj16i9qh25dw9q1y69ri619n"))))
    (build-system cmake-build-system)
    (native-inputs
     (list docbook-xml
           docbook-xsl
           glib
           libxml2
           libxslt
           pkg-config
           xmlto))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cups" ,cups)
       ("ffmpeg" ,ffmpeg)
       ("libjpeg" ,libjpeg-turbo)
       ("libusb" ,libusb)
       ("libx11" ,libx11)
       ("libxkbfile" ,libxkbfile)
       ("libxcursor" ,libxcursor)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxv" ,libxv)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxinerama" ,libxinerama)
       ("libxshmfence" ,libxshmfence)
       ("pulseaudio" ,pulseaudio)
       ("zlib" ,zlib)))
    (propagated-inputs
     (list libxkbcommon openssl wayland))
    (arguments
     `(#:build-type "RELEASE"
       #:configure-flags
       (list "-DWITH_JPEG=ON"
             ,@(if (string-prefix? "x86_64"
                                   (or (%current-target-system)
                                       (%current-system)))
                   '("-DWITH_SSE2=ON")
                   '())
             (string-append "-DDOCBOOKXSL_DIR="
                            (assoc-ref %build-inputs "docbook-xsl")
                            "/xml/xsl/docbook-xsl-"
                            ,(package-version docbook-xsl))
             "-DWITH_PULSE=ON"
             "-DWITH_CUPS=ON"
             "-DBUILD_TESTING=ON")))
    (home-page "https://www.freerdp.com")
    (synopsis "Remote Desktop Protocol implementation")
    (description "FreeRDP implements Microsoft's Remote Desktop Protocol.
It consists of the @code{xfreerdp} client, libraries for client and server
functionality, and Windows Portable Runtime (WinPR), a portable implementation
of parts of the Windows API.")
    (license license:asl2.0)))
