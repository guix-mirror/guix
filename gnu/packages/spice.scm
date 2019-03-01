;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
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

(define-module (gnu packages spice)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils))

(define-public usbredir
  (package
    (name "usbredir")
    (home-page "https://spice-space.org")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/download/" name "/" name "-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "002yik1x7kn0427xahvnhjby2np14a6xqw7c3dx530n9h5d9rg47"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libusb" ,libusb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Tools for sending USB device traffic over a network")
    (description
     "Usbredir is a network protocol for sending USB device traffic over a
network connection.  It can be used to redirect traffic from a USB device to a
different (virtual) machine than the one to which the USB device is attached.")
    (license (list license:gpl2+ license:lgpl2.0+ license:lgpl2.1+))))

(define-public virglrenderer
  (package
    (name "virglrenderer")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "https://www.freedesktop.org/software/virgl/"
                "virglrenderer-" version ".tar.bz2"))
              (patches (search-patches "virglrenderer-CVE-2017-6386.patch"))
              (sha256
               (base32
                "06kf0q4l52gzx5p63l8850hff8pmhp7xv1hk8zgx2apbw18y6jd5"))))
    (build-system gnu-build-system)
    (inputs
      `(("libepoxy" ,libepoxy)
        ("mesa" ,mesa)
        ("udev" ,eudev)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (synopsis "Virtual 3D GPU library")
    (description "A virtual 3D GPU library that enables a virtualized operating
system to use the host GPU to accelerate 3D rendering.")
    (home-page "https://virgil3d.github.io")
    (license (list license:expat license:bsd-3))))

(define-public spice-protocol
  (package
    (name "spice-protocol")
    (version "0.12.15")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "https://www.spice-space.org/download/releases/"
                "spice-protocol-" version ".tar.bz2"))
              (sha256
               (base32
                "06b461i4jv741in8617jjpfk28wk7zs9p7841njkf4sbm8xv4kcb"))))
    (build-system gnu-build-system)
    (synopsis "Protocol headers for the SPICE protocol")
    (description "SPICE (the Simple Protocol for Independent Computing
Environments) is a remote-display system built for virtual environments
which allows users to view a desktop computing environment.")
    (home-page "https://www.spice-space.org")
    (license (list license:bsd-3 license:lgpl2.1+))))

(define-public spice-gtk
  (package
    (name "spice-gtk")
    (version "0.35")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "https://spice-space.org/download/gtk/"
                "spice-gtk-" version ".tar.bz2"))
              (sha256
               (base32
                "11lymg467gvj5ys8k22ihnfbxjn4x34ygyzirpg2nphjwlyhgrml"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("gstreamer" ,gstreamer)
        ("gst-libav" ,gst-libav)
        ("gst-plugins-base" ,gst-plugins-base)
        ("gst-plugins-good" ,gst-plugins-good)
        ("gst-plugins-bad" ,gst-plugins-bad)
        ("gst-plugins-ugly" ,gst-plugins-ugly)
        ("spice-protocol" ,spice-protocol)))
    (inputs
      `(("glib-networking" ,glib-networking)
        ("gobject-introspection" ,gobject-introspection)
        ("gtk+" ,gtk+)
        ("libepoxy" ,libepoxy)
        ("libjpeg" ,libjpeg)
        ("libxcb" ,libxcb)
        ("lz4" ,lz4)
        ("mesa" ,mesa)
        ("pixman" ,pixman)
        ("pulseaudio" ,pulseaudio)
        ("python" ,python)
        ("openssl" ,openssl)
        ("opus" ,opus)
        ("usbredir" ,usbredir)))
    (native-inputs
      `(("glib:bin" ,glib "bin")
        ("intltool" ,intltool)
        ("pkg-config" ,pkg-config)))
    (arguments
      `(#:configure-flags
        '("--enable-gstaudio"
          "--enable-gstvideo"
          "--enable-pulse"
          "--enable-introspection")
        #:phases
         (modify-phases %standard-phases
           (add-after
            'install 'wrap-spicy
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out             (assoc-ref outputs "out"))
                    (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
                (wrap-program (string-append out "/bin/spicy")
                  `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
              #t)))))
    (synopsis "Gtk client and libraries for SPICE remote desktop servers")
    (description "Gtk client and libraries for SPICE remote desktop servers.")
    (home-page "https://www.spice-space.org")
    (license (list license:lgpl2.1+ license:lgpl2.0+))))

(define-public spice
  (package
    (name "spice")
    (version "0.14.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "https://www.spice-space.org/download/releases/"
                "spice-server/spice-" version ".tar.bz2"))
              (sha256
               (base32
                "068mb9l7wzk4k4c65bzvpw5fyyzh81rb6z81skgdxvh67pk5vb8y"))))
    (build-system gnu-build-system)
    (propagated-inputs
      `(("openssl" ,openssl)
        ("pixman" ,pixman)
        ("spice-protocol" ,spice-protocol)))
    (inputs
      `(("cyrus-sasl" ,cyrus-sasl)
        ("glib" ,glib)
        ("libjpeg-turbo" ,libjpeg-turbo)
        ("lz4" ,lz4)
        ("opus" ,opus)
        ("orc" ,orc)
        ("zlib" ,zlib)))
    (native-inputs
      `(("pkg-config" ,pkg-config)
        ("python" ,python)
        ("spice-gtk" ,spice-gtk)

        ;; These are needed for the server listen tests.
        ("glib-networking" ,glib-networking)
        ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (arguments
      `(#:configure-flags
        '("--enable-lz4"
          "--enable-automated-tests")

        ;; Several tests appear to be opening the same sockets concurrently.
        #:parallel-tests? #f))
    (synopsis "Server implementation of the SPICE protocol")
    (description "SPICE is a remote display system built for virtual
environments which allows you to view a computing 'desktop' environment
not only on the machine where it is running, but from anywhere on the
Internet and from a wide variety of machine architectures.")
    (home-page "https://www.spice-space.org")
    (license (list license:lgpl2.1+ license:lgpl2.0+))))

(define-public spice-vdagent
  (package
    (name "spice-vdagent")
    (version "0.17.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "http://www.spice-space.org/download/releases/"
                "spice-vdagent-" version ".tar.bz2"))
              (sha256
               (base32
                "0gdkyylyg1hksg0i0anvznqfli2q39335fnrmcd6847frpc8njpi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile.in
           (lambda _
             (substitute* "Makefile.in"
               (((string-append "\\$\\(mkdir_p\\) \\$\\(DESTDIR\\)"
                                "\\$\\(localstatedir\\)/run/spice-vdagentd"))
                 "-$(mkdir_p) $(DESTDIR)$(localstatedir)/run/spice-vdagentd"))
             #t))
         (add-after 'unpack 'patch-spice-vdagent.desktop
           (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "data/spice-vdagent.desktop"
              (("Exec=/usr/bin/spice-vdagent")
               (string-append "Exec=" (assoc-ref outputs "out")
                              "/bin/spice-vdagent")))
             #t)))))
    (inputs
      `(("alsa-lib" ,alsa-lib)
        ("dbus" ,dbus)
        ("glib" ,glib)
        ("libpciaccess" ,libpciaccess)
        ("libx11" ,libx11)
        ("libxext" ,libxext)
        ("libxfixes" ,libxfixes)
        ("libxinerama" ,libxinerama)
        ("libxrandr" ,libxrandr)
        ("spice-protocol" ,spice-protocol)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (synopsis "Spice agent for Linux")
    (description "Spice-vdagent enables sharing the clipboard and guest display
resolution scaling on graphical console window resize.")
    (home-page "https://www.spice-space.org")
    (license license:gpl3+)))

(define-public virt-viewer
  (package
    (name "virt-viewer")
    (version "7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "https://virt-manager.org/download/sources/virt-viewer/"
                "virt-viewer-" version ".tar.gz"))
              (sha256
               (base32
                "00y9vi69sja4pkrfnvrkwsscm41bqrjzvp8aijb20pvg6ymczhj7"))))
    (build-system gnu-build-system)
    (inputs
      `(("gtk+" ,gtk+)
        ("libcap" ,libcap)
        ("libxml2" ,libxml2)
        ("openssl" ,openssl)
        ("spice-gtk" ,spice-gtk)))
    (native-inputs
      `(("glib:bin" ,glib "bin")
        ("intltool" ,intltool)
        ("pkg-config" ,pkg-config)))
    (arguments
      `(#:configure-flags
        '("--with-spice-gtk")
        #:phases
         (modify-phases %standard-phases
           (add-after
            'install 'wrap-remote-viewer
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out             (assoc-ref outputs "out"))
                    (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
                (wrap-program (string-append out "/bin/remote-viewer")
                  `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
              #t)))))
    (synopsis "Graphical console client for virtual machines")
    (description "Graphical console client for virtual machines using SPICE or
VNC.")
    (home-page "https://virt-manager.org")
    (license license:gpl2+)))
