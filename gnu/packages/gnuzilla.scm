;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer-0.10)
       ("gst-plugins-base" ,gst-plugins-base-0.10)
       ("gtk+" ,gtk+-2)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2) ; Python 3 not supported
       ("python2-pysqlite" ,python2-pysqlite)
       ("unzip" ,unzip)
       ("yasm" ,yasm)
       ("zip" ,zip)))
    (arguments
     `(#:tests? #f ; no check target
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
              (zero? (system* "./configure"
                              (string-append "--prefix=" out)
                              "--disable-webrtc")))) ; webrtc creates an error
          %standard-phases))))
    (home-page "http://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons. It also
features extra privacy-protecting features built in.")
    (license license:mpl2.0))) ; and others, see toolkit/content/license.html
