;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml))

(define-public rdesktop
  (package
    (name "rdesktop")
    (version "1.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/" name "/" name "/" version "/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r7c1rjmw2xzq8fw0scyb453gy9z19774z1z8ldmzzsfndb03cl8"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list (string-append "--with-openssl="
                                              (assoc-ref %build-inputs
                                                         "openssl"))

                               ;; XXX: optional dependencies missing
                               "--disable-credssp"
                               "--disable-smartcard")

       #:tests? #f))                              ;no 'check' target
    (inputs
     `(("libx11" ,libx11)
       ("openssl" ,openssl)))
    (home-page "http://www.rdesktop.org/")
    (synopsis "Client for Windows Terminal Services")
    (description
     "rdesktop is a client for Microsoft's Windows Remote Desktop Services,
capable of natively speaking Remote Desktop Protocol (RDP).  It allows users
to remotely control a user's Windows desktop.")
    (license license:gpl3+)))

(define-public freerdp
  (let ((commit "03ab68318966c3a22935a02838daaea7b7fbe96c"))
    (package
      (name "freerdp")
      (version (git-version "1.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;; We need the 1.1 branch for RDP support in vinagre.
                      (url "git://github.com/FreeRDP/FreeRDP.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "07ish8rmvbk2zd99k91qybmmh5h4afly75l5kbvslhq1r6k8pbmp"))))
      (build-system cmake-build-system)
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("libxslt" ,libxslt)
         ("libxml2" ,libxml2)
         ("docbook-xsl" ,docbook-xsl)
         ("xmlto" ,xmlto)))
      (inputs
       `(("libx11" ,libx11)
         ("libxkbfile" ,libxkbfile)
         ("libxcursor" ,libxcursor)
         ("libxext" ,libxext)
         ("libxi" ,libxi)
         ("libxv" ,libxv)
         ("libxrandr" ,libxrandr)
         ("libxrender" ,libxrender)
         ("libxinerama" ,libxinerama)
         ("libxshmfence" ,libxshmfence)
         ("cups" ,cups)
         ("ffmpeg" ,ffmpeg-2.8)
         ("libjpeg" ,libjpeg)
         ("pulseaudio" ,pulseaudio)
         ("alsa-lib" ,alsa-lib)
         ("zlib" ,zlib)
         ("openssl" ,openssl)))
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
               "-DWITH_CUPS=ON")
         #:tests? #f))                              ; no 'test' target
      (home-page "https://www.freerdp.com")
      (synopsis "Remote Desktop Protocol implementation")
      (description "FreeRDP implements Microsoft's Remote Desktop Protocol.
It consists of the @code{xfreerdp} client, libraries for client and server
functionality, and Windows Portable Runtime (WinPR), a portable implementation
of parts of the Windows API.")
    (license license:asl2.0))))
