;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages gnuzilla)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages yasm)
  #:use-module (gnu packages zip))

(define-public icecat
  (package
    (name "icecat")
    (version "31.2.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnuzilla/"
                          version "/" name "-" version ".tar.xz"))
      (sha256
       (base32
        "02r9klfc0z26w270inq652249hq0wfzvwhzvwmk0n8v8nzkk5idh"))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer-0.10)
       ("gst-plugins-base" ,gst-plugins-base-0.10)
       ("gtk+" ,gtk+-2)
       ("pango" ,pango)
       ("freetype" ,freetype)
       ("libxft" ,libxft)
       ("libevent" ,libevent)
       ("libxt" ,libxt)
       ("libffi" ,libffi)
       ("pulseaudio" ,pulseaudio)
       ("mesa" ,mesa)
       ("unzip" ,unzip)
       ("yasm" ,yasm)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2) ; Python 3 not supported
       ("python2-pysqlite" ,python2-pysqlite)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #f          ; no check target
       #:out-of-source? #t  ; must be built outside of the source directory

       #:configure-flags '(;; Building with debugging symbols takes ~5GiB, so
                           ;; disable it.
                           "--disable-debug"
                           "--disable-debug-symbols"

                           "--disable-webrtc"     ; webrtc fails to build

                           "--with-system-zlib"
                           "--with-system-bz2"    ; FIXME: not used
                           "--with-system-libevent"

                           ;; Fails with "--with-system-png won't work because
                           ;; the system's libpng doesn't have APNG support".
                           ;; According to
                           ;; http://sourceforge.net/projects/libpng-apng/ ,
                           ;; "the Animated Portable Network Graphics (APNG)
                           ;; is an unofficial extension of the Portable
                           ;; Network Graphics (PNG) format";
                           ;; we probably do not wish to support it.
                           ;; "--with-system-png"

                           ;; Fails with "libjpeg-turbo JCS_EXTENSIONS
                           ;; required".
                           ;; According to
                           ;; http://sourceforge.net/projects/libjpeg-turbo/ ,
                           ;; "libjpeg-turbo is a derivative of libjpeg that
                           ;; uses MMX, SSE, SSE2, and NEON SIMD instructions
                           ;; to accelerate baseline JPEG compression/
                           ;; decompression", so we had better not use it
                           ;; "--with-system-jpeg"

                           "--enable-system-ffi")

       #:phases
       (alist-replace
        'configure
        ;; configure does not work followed by both "SHELL=..." and
        ;; "CONFIG_SHELL=..."; set environment variables instead
        (lambda* (#:key outputs configure-flags #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (bash (which "bash"))
                 (abs-srcdir (getcwd))
                 (srcdir (string-append "../" (basename abs-srcdir)))
                 (flags `(,(string-append "--prefix=" out)
                          ,(string-append "--with-l10n-base="
                                          abs-srcdir "/l10n")
                          ,@configure-flags)))
            (setenv "SHELL" bash)
            (setenv "CONFIG_SHELL" bash)
            (mkdir "../build")
            (chdir "../build")
            (format #t "build directory: ~s~%" (getcwd))
            (format #t "configure flags: ~s~%" flags)
            (zero? (apply system* bash
                          (string-append srcdir "/configure")
                          flags))))
        %standard-phases)))
    (home-page "http://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.")
    (license license:mpl2.0))) ; and others, see toolkit/content/license.html
