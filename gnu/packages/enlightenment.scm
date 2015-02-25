;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Tomáš Čech <sleep_walker@suse.cz>
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

(define-module (gnu packages efl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xorg))

(define-public efl
  (package
    (name "efl")
    (version "1.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.enlightenment.org/rel/libs/efl/efl-"
                    version ".tar.gz"))
              (sha256
               (base32 "0mwkxsfi5masiy6qfmdqy1j15cfz7nz8g8jq2bf3w501rpa9gxvz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("compositeproto" ,compositeproto)
       ("curl" ,curl)
       ("giflib" ,giflib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("harfbuzz" ,harfbuzz)
       ("libexif" ,libexif)
       ("libjpeg" ,libjpeg)
       ("librsvg" ,librsvg)
       ("libtiff" ,libtiff)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxdmcp" ,libxdmcp)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxkbfile" ,libxkbfile)
       ("libxinerama" ,libxinerama)
       ("libxp" ,libxp)
       ("libxrandr" ,libxrandr)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxtst" ,libxtst)
       ("mesa" ,mesa)
       ("printproto" ,printproto)
       ("scrnsaverproto" ,scrnsaverproto)
       ("valgrind" ,valgrind)
       ("xextproto" ,xextproto)
       ("xinput" ,xinput)
       ("xpr" ,xpr)
       ("xproto" ,xproto)))
    (propagated-inputs
     ;; All these inputs are in package config files in section
     ;; Require.private.
     `(("bullet" ,bullet) ; ephysics.pc
       ("dbus" ,dbus) ; eldbus.pc
       ("eudev" ,eudev) ; eeze.pc
       ("fontconfig" ,fontconfig) ; evas.pc
       ("freetype" ,freetype) ; evas.pc
       ("fribidi" ,fribidi) ; evas.pc
       ("glib" ,glib) ; ecore.pc
       ("libpng" ,libpng) ; evas.pc, evas-cxx.pc
       ("libsndfile" ,libsndfile) ; ecore-audio.pc, ecore-audio-cxx.pc
       ("luajit" ,luajit) ; evas.pc, edje.pc
       ("openssl" ,openssl) ; eet.pc, ecore-con.pc
       ("pulseaudio" ,pulseaudio) ; ecore-audio.pc, ecore-audio-cxx.pc
       ("util-linux" ,util-linux) ; eeze.pc
       ("zlib" ,zlib))) ; eet.pc
    (arguments
     `(#:configure-flags '("--disable-silent-rules")
       #:phases
       (alist-cons-before
        'configure 'patch-config-files
        (lambda _
          (substitute* "po/Makefile.in.in"
            (("/bin/sh") (which "bash"))))
        %standard-phases)))
    (home-page "http://www.enlightenment.org")
    (synopsis "Enlightenment Foundation Libraries")
    (description
     "Enlightenment Foundation Libraries is a set of libraries developed
for Enlightenment.  Libraries covers data serialization, wide support for
graphics rendering, UI layout and themes, interaction with OS, access to
removable devices or support for multimedia.")
    (license (list license:bsd-2 license:lgpl2.1 license:zlib)))) ; different parts under different licenses
