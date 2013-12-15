;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
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
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages yasm)
  #:use-module (gnu packages zip))

(define-public icecat
  (package
    (name "icecat")
    (version "24.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnuzilla/"
                          (substring version 0 (string-index version #\.))
                          "/icecat-" version ".tar.gz"))
      (sha256
       (base32
        "1vxzjwmhad6yxx4sk9zvapjgv5salcv10id061q0991ii3dycy9a"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer-0.10)
       ("gst-plugins-base" ,gst-plugins-base-0.10)
       ("gtk+" ,gtk+-2)
       ("libevent" ,libevent)
       ("libxt" ,libxt)
       ("libffi" ,libffi)
       ("mesa" ,mesa)
       ("unzip" ,unzip)
       ("yasm" ,yasm)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2) ; Python 3 not supported
       ("python2-pysqlite" ,python2-pysqlite)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2))) ; Python 3 not supported
    (arguments
     `(#:tests? #f ; no check target

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
         (alist-cons-before
          'patch-source-shebangs 'sanitise
          (lambda _
            ;; delete dangling symlinks
            (delete-file "browser/base/content/.#aboutDialog.xul")
            (delete-file "browser/base/content/abouthome/.#aboutHome.xhtml")
            (delete-file "browser/branding/unofficial/content/.#aboutHome.xhtml")
            (delete-file "toolkit/crashreporter/google-breakpad/autotools/compile"))
         (alist-replace
          'configure
          ;; configure does not work followed by both "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key outputs configure-flags #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (setenv "SHELL" (which "bash"))
              (setenv "CONFIG_SHELL" (which "bash"))
              (zero? (apply system* "./configure"
                            (string-append "--prefix=" out)
                            configure-flags))))
          %standard-phases))))
    (home-page "http://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons. It also
features built-in privacy-protecting features.")
    (license license:mpl2.0))) ; and others, see toolkit/content/license.html
