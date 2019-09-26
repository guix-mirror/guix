;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public libwpe
  (package
    (name "libwpe")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wpewebkit.org/releases/libwpe-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1221vs72zs87anrzhbm6pf8jnii7s6ms7mkzj6nlds9zqd7lklz2"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("mesa" ,mesa)))
    (propagated-inputs
     `(;; In Requires of wpe-1.0.pc.
       ("libxkbcommon" ,libxkbcommon)))
    (home-page "https://wpewebkit.org/")
    (synopsis "Platform agnostic WebKit interfaces")
    (description
     "@code{libwpe} is a small library that defines programming interfaces
for use by WebKit, and provides a mechanism for loading a platform-specific
backend which implements them.")
    (license license:bsd-2)))

(define-public wpebackend-fdo
  (package
    (name "wpebackend-fdo")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wpewebkit.org/releases/"
                                  "wpebackend-fdo-" version ".tar.xz"))
              (sha256
               (base32
                "1bwbs47v4nlzhsqrw9fpyny5m3n9ry0kfzsvk90zjif4bd5cl6d9"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("libwpe" ,libwpe)
       ("mesa" ,mesa)
       ("wayland" ,wayland)))
    (home-page "https://wpewebkit.org/")
    (synopsis "Wayland WPE backend")
    (description
     "This package provides a backend implementation for the WPE WebKit
engine that uses Wayland for graphics output.")
    (license license:bsd-2)))

(define-public webkitgtk
  (package
    (name "webkitgtk")
    (version "2.20.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.webkitgtk.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "147r7an41920zl4x9srdva7fxvw2znjin5ldjkhay1cndv9gih0m"))))
    (build-system cmake-build-system)
    (outputs '("out" "doc"))
    (arguments
     '(#:tests? #f ; no tests
       #:build-type "Release" ; turn off debugging symbols to save space
       #:configure-flags (list
                          "-DPORT=GTK"
                          "-DENABLE_GTKDOC=ON" ; No doc by default
                          (string-append ; uses lib64 by default
                           "-DLIB_INSTALL_DIR="
                           (assoc-ref %outputs "out") "/lib")

                          ;; XXX Adding GStreamer GL support would apparently
                          ;; require adding gst-plugins-bad to the inputs,
                          ;; which might entail a security risk as a result of
                          ;; the plugins of dubious code quality that are
                          ;; included.  More investigation is needed.  For
                          ;; now, we explicitly disable it to prevent an error
                          ;; at configuration time.
                          "-DUSE_GSTREAMER_GL=OFF"

                          ;; XXX Disable WOFF2 ‘web fonts’.  These were never
                          ;; supported in our previous builds.  Enabling them
                          ;; requires building libwoff2 and possibly woff2dec.
                          "-DUSE_WOFF2=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-gtk-doc-scan
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (file)
                         (substitute* file
                           (("http://www.oasis-open.org/docbook/xml/4.1.2/docbookx.dtd")
                            (string-append (assoc-ref inputs "docbook-xml")
                                           "/xml/dtd/docbook/docbookx.dtd"))))
                       (find-files "Source" "\\.sgml$"))
             #t))
         (add-after 'install 'move-doc-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file (string-append out "/share/gtk-doc")
                            (string-append doc "/share/gtk-doc"))
               #t))))))
    (native-inputs
     `(("bison" ,bison)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("gperf" ,gperf)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2) ; incompatible with Python 3 (print syntax)
       ("gtk-doc" ,gtk-doc) ; For documentation generation
       ("docbook-xml" ,docbook-xml) ; For documentation generation
       ("ruby" ,ruby)))
    (propagated-inputs
     `(("gtk+" ,gtk+)
       ("libsoup" ,libsoup)))
    (inputs
     `(("at-spi2-core" ,at-spi2-core)
       ("enchant" ,enchant)
       ("geoclue" ,geoclue)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+-2" ,gtk+-2)
       ("harfbuzz" ,harfbuzz)
       ("hyphen" ,hyphen)
       ("icu4c" ,icu4c)
       ("libgcrypt" ,libgcrypt)
       ("libjpeg" ,libjpeg)
       ("libnotify" ,libnotify)
       ("libpng" ,libpng)
       ("libsecret" ,libsecret)
       ("libtasn1" ,libtasn1)
       ("libwebp" ,libwebp)
       ("libxcomposite" ,libxcomposite)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("sqlite" ,sqlite)))
    (home-page "https://www.webkitgtk.org/")
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

;; This version of webkitgtk needs to be kept separate, because it requires a
;; newer version of GCC than our default compiler, and this causes problems
;; when linked with C++ libraries built using our default compiler.  For now,
;; we use this newer webkitgtk only for selected packages, e.g. epiphany.
(define-public webkitgtk-2.26
  (package/inherit webkitgtk
    (name "webkitgtk")
    (version "2.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.webkitgtk.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0mfikjfjhwcnrxbzdyh3fl9bbs2azgbdnx8h5910h41b3n022jvb"))))
    (native-inputs
     `(("gcc" ,gcc-7)  ; webkitgtk-2.22 requires gcc-6 or newer
       ,@(package-native-inputs webkitgtk)))
    (inputs
     `(("bubblewrap" ,bubblewrap)
       ("libseccomp" ,libseccomp)
       ("libwpe" ,libwpe)
       ("openjpeg" ,openjpeg)
       ("wpebackend-fdo" ,wpebackend-fdo)
       ("xdg-dbus-proxy" ,xdg-dbus-proxy)
       ,@(package-inputs webkitgtk)))
    (arguments
     (substitute-keyword-arguments (package-arguments webkitgtk)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'work-around-gcc-7-include-path-issue
             ;; FIXME: Work around a problem with gcc-7 includes (see
             ;; <https://bugs.gnu.org/30756>).
             (lambda _
               (unsetenv "C_INCLUDE_PATH")
               (unsetenv "CPLUS_INCLUDE_PATH")
               #t))))))))
