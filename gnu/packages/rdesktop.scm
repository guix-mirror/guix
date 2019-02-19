;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages linux)
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
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rdesktop/rdesktop/"
                                  "releases/download/v" version "/rdesktop-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0bfd9nl2dfr1931fv6bpnrj5yf88ikijrs4s3nm96gm87bkvi64v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--with-openssl="
                                              (assoc-ref %build-inputs
                                                         "openssl"))

                               ;; XXX: optional dependencies missing
                               "--disable-credssp"
                               "--disable-smartcard")

       #:phases
       (modify-phases %standard-phases
         (add-after 'install-license-files 'delete-extraneous-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (license-dir (string-append out "/share/doc/"
                                                ,name "-" ,version)))
               ;; XXX These files are installed erroneously.
               (delete-file (string-append license-dir "/licence.c"))
               (delete-file (string-append license-dir "/licence.o")))
             #t)))
       #:tests? #f))                              ;no 'check' target
    (inputs
     `(("libx11" ,libx11)
       ("openssl" ,openssl)))
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
    (version "2.0.0-rc4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "git://github.com/FreeRDP/FreeRDP.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0546i0m2d4nz5jh84ngwzpcm3c43fp987jk6cynqspsmvapab6da"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("glib" ,glib)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)
       ("xmlto" ,xmlto)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cups" ,cups)
       ("ffmpeg" ,ffmpeg)
       ("libjpeg" ,libjpeg)
       ("libx11" ,libx11)
       ("libxkbcommon" ,libxkbcommon)
       ("libxkbfile" ,libxkbfile)
       ("libxcursor" ,libxcursor)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxv" ,libxv)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxinerama" ,libxinerama)
       ("libxshmfence" ,libxshmfence)
       ("openssl" ,openssl)
       ("pulseaudio" ,pulseaudio)
       ("wayland" ,wayland)
       ("zlib" ,zlib)))
    (arguments
     `(#:build-type "RELEASE"
       #:configure-flags
       (list "-DWITH_JPEG=ON"
             "-DWITH_LIBSYSTEMD=OFF"
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
