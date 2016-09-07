;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages webkit)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public webkitgtk
  (package
    (name "webkitgtk")
    (version "2.12.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.webkitgtk.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0h0wig413399wws6l88mn1nnjbqb42vb55yvz8az39b4p1a7h53b"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:build-type "Release" ; turn off debugging symbols to save space
       #:configure-flags (list
                          "-DPORT=GTK"
                          (string-append ; uses lib64 by default
                           "-DLIB_INSTALL_DIR="
                           (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after
          'set-paths 'add-gst-plugins-base-include-path
          (lambda* (#:key inputs #:allow-other-keys)
            ;; XXX Work around a problem in the build system, which neglects
            ;; to add -I for gst-plugins-base when compiling
            ;; Source/WebKit2/UIProcess/WebPageProxy.cpp, apparently assuming
            ;; that it will be in the same directory as gstreamer's header
            ;; files.
            (setenv "CPATH"
                    (string-append (getenv "C_INCLUDE_PATH")
                                   ":"
                                   (assoc-ref inputs "gst-plugins-base")
                                   "/include/gstreamer-1.0")))))))
    (native-inputs
     `(("bison" ,bison)
       ("gettext" ,gnu-gettext)
       ("glib:bin" ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2) ; incompatible with Python 3 (print syntax)
       ("ruby" ,ruby)))
    (propagated-inputs
     `(("gtk+" ,gtk+)
       ("libsoup" ,libsoup)))
    (inputs
     `(("at-spi2-core" ,at-spi2-core)
       ("enchant" ,enchant)
       ("geoclue" ,geoclue)
       ("gnutls" ,gnutls)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+-2" ,gtk+-2)
       ("harfbuzz" ,harfbuzz)
       ("hyphen" ,hyphen)
       ("icu4c" ,icu4c)
       ("libjpeg" ,libjpeg)
       ("libnotify" ,libnotify)
       ("libpng" ,libpng)
       ("libsecret" ,libsecret)
       ("libwebp" ,libwebp)
       ("libxcomposite" ,libxcomposite)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("sqlite" ,sqlite)))
    (home-page "http://www.webkitgtk.org/")
    (synopsis "Web content engine for GTK+")
    (description
     "WebKitGTK+ is a full-featured port of the WebKit rendering engine,
suitable for projects requiring any kind of web integration, from hybrid
HTML/CSS applications to full-fledged web browsers.")
    ;; WebKit's JavaScriptCore and WebCore components are available under
    ;; the GNU LGPL, while the rest is available under a BSD-style license.
    (license (list license:lgpl2.0
                   license:lgpl2.1+
                   license:bsd-2
                   license:bsd-3))))

;; Latest release of the stable 2.4 series, with WebKit1 support.
(define-public webkitgtk-2.4
  (package (inherit webkitgtk)
    (name "webkitgtk")
    (version "2.4.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.webkitgtk.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1xsvnvyvlywwyf6m9ainpsg87jkxjmd37q6zgz9cxb7v3c2ym2jq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       ;; FIXME: Disabling parallel building is a quick hack to avoid the
       ;; failure described in
       ;; https://lists.gnu.org/archive/html/guix-devel/2016-01/msg00837.html
       ;; A more structural fix is needed.
       #:parallel-build? #f
       #:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'set-gcc
                   (lambda _ (setenv "CC" "gcc") #t)))))
    (native-inputs
     `(("flex" ,flex)
       ("which" ,which)
       ,@(package-native-inputs webkitgtk)))))

;; Last GTK+2 port, required by GnuCash.
(define-public webkitgtk/gtk+-2
  (package (inherit webkitgtk-2.4)
    (name "webkitgtk-gtk2")
    (arguments
     `(;; FIXME: Disabling parallel building is a quick hack to avoid the
       ;; failure described in
       ;; https://lists.gnu.org/archive/html/guix-devel/2016-01/msg00837.html
       ;; A more structural fix is needed.
       #:parallel-build? #f
       #:configure-flags
       '("--enable-webkit2=no"
         "--with-gtk=2.0")
       ,@(package-arguments webkitgtk-2.4)))
    (propagated-inputs
     `(("gtk+-2" ,gtk+-2)
       ("libsoup" ,libsoup)))))
