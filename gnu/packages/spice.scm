;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
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
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils))

(define-public usbredir
  (package
    (name "usbredir")
    (home-page "https://spice-space.org")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/download/" name "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "19jnpzlanq0a1m5lmlcsp50wxf7icxvpvclx7hnf0zxw8azngqd3"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list libusb))
    (native-inputs
     (list autoconf automake libtool pkg-config))
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
      (list libepoxy mesa eudev))
    (native-inputs
      (list pkg-config))
    (synopsis "Virtual 3D GPU library")
    (description "A virtual 3D GPU library that enables a virtualized operating
system to use the host GPU to accelerate 3D rendering.")
    (home-page "https://virgil3d.github.io")
    (license (list license:expat license:bsd-3))))

(define-public spice-protocol
  (package
    (name "spice-protocol")
    (version "0.14.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "https://www.spice-space.org/download/releases/"
                "spice-protocol-" version ".tar.xz"))
              (sha256
               (base32
                "0yj8k7gcirrsf21w0q6146n5g4nzn2pqky4p90n5760m5ayfb1pr"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/"
                                        ,name "-" ,version)))
               (install-file "COPYING" doc)
               #t))))))
    (synopsis "Protocol headers for the SPICE protocol")
    (description "SPICE (the Simple Protocol for Independent Computing
Environments) is a remote-display system built for virtual environments
which allows users to view a desktop computing environment.")
    (home-page "https://www.spice-space.org")
    (license (list license:bsd-3 license:lgpl2.1+))))

(define-public spice-gtk
  (package
    (name "spice-gtk")
    (version "0.37")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "https://spice-space.org/download/gtk/"
                "spice-gtk-" version ".tar.bz2"))
              (sha256
               (base32
                "1drvj8y35gnxbnrxsipwi15yh0vs9ixzv4wslz6r3lra8w3bfa0z"))))
    (build-system gnu-build-system)
    (propagated-inputs
      (list gstreamer
            gst-plugins-base
            gst-plugins-good
            spice-protocol
            ;; These are required by the pkg-config files.
            gtk+
            pixman
            openssl))
    (inputs
      `(("glib-networking" ,glib-networking)
        ("gobject-introspection" ,gobject-introspection)
        ("json-glib" ,json-glib)
        ("libepoxy" ,libepoxy)
        ("libjpeg" ,libjpeg-turbo)
        ("libxcb" ,libxcb)
        ("lz4" ,lz4)
        ("mesa" ,mesa)
        ("pulseaudio" ,pulseaudio)
        ("python" ,python)
        ("opus" ,opus)
        ("usbredir" ,usbredir)))
    (native-inputs
      `(("glib:bin" ,glib "bin")
        ("intltool" ,intltool)
        ("pkg-config" ,pkg-config)
        ("vala" ,vala)))
    (arguments
      `(#:configure-flags
        '("--enable-gstaudio"
          "--enable-gstvideo"
          "--enable-pulse"
          "--enable-vala"
          "--enable-introspection")
        #:phases
         (modify-phases %standard-phases
           (add-before 'check 'disable-session-test
             (lambda _
               ;; XXX: Disable session tests, because they require USB support,
               ;; which is not available in the build container.
               (substitute* "tests/Makefile"
                 (("test-session\\$\\(EXEEXT\\) ") ""))
               #t))
           (add-after 'install 'patch-la-files
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (libjpeg (assoc-ref inputs "libjpeg")))
                 ;; Add an absolute reference for libjpeg in the .la files
                 ;; so it does not have to be propagated.
                 (substitute* (find-files (string-append out "/lib") "\\.la$")
                   (("-ljpeg")
                    (string-append "-L" libjpeg "/lib -ljpeg")))
                 #t)))
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
    (version "0.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "https://www.spice-space.org/download/releases/"
                "spice-server/spice-" version ".tar.bz2"))
              (sha256
               (base32
                "1xd0xffw0g5vvwbq4ksmm3jjfq45f9dw20xpmi82g1fj9f7wy85k"))))
    (build-system gnu-build-system)
    (propagated-inputs
      (list openssl pixman spice-protocol))
    (inputs
      (list cyrus-sasl
            glib
            libjpeg-turbo
            libcacard ; smartcard support
            lz4
            opus
            orc
            zlib))
    (native-inputs
      (list pkg-config
            python
            spice-gtk
            ;; These are needed for the server listen tests.
            glib-networking
            gsettings-desktop-schemas))
    (arguments
      `(#:configure-flags
        '("--enable-lz4"
          "--enable-automated-tests")

        #:phases
        (modify-phases %standard-phases
          ;; XXX: Otherwise the server listen tests fails with
          ;;   Failed to create /homeless-shelter/.config/glib-2.0/settings
          (add-before 'check 'set-XDG_CONFIG_HOME
            (lambda _
              (setenv "XDG_CONFIG_HOME" "/tmp"))))

        ;; Several tests appear to be opening the same sockets concurrently.
        #:parallel-tests? #f))
    (synopsis "Server implementation of the SPICE protocol")
    (description "SPICE is a remote display system built for virtual
environments which allows you to view a computing @code{desktop} environment
not only on the machine where it is running, but from anywhere on the
Internet and from a wide variety of machine architectures.")
    (home-page "https://www.spice-space.org")
    (license (list license:lgpl2.1+ license:lgpl2.0+))))

(define-public spice-vdagent
  (package
    (name "spice-vdagent")
    (version "0.21.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "http://www.spice-space.org/download/releases/"
                "spice-vdagent-" version ".tar.bz2"))
              (sha256
               (base32
                "0n8jlc1pv6mkry161y656b1nk9hhhminjq6nymzmmyjl7k95ymzx"))
              (patches
               (search-patches "spice-vdagent-glib-2.68.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--localstatedir=/var")
       ;; The test-session-info test fails for unknown reasons (see:
       ;; https://gitlab.freedesktop.org/spice/linux/vd_agent/-/issues/24).
       #:make-flags '("XFAIL_TESTS=tests/test-session-info")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile.in
           (lambda _
             (substitute* "Makefile.in"
               (((string-append "\\$\\(mkdir_p\\) \\$\\(DESTDIR\\)"
                                "\\$\\(localstatedir\\)/run/spice-vdagentd"))
                 "-$(mkdir_p) $(DESTDIR)$(localstatedir)/run/spice-vdagentd"))))
         (add-after 'unpack 'patch-spice-vdagent.desktop
           (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "data/spice-vdagent.desktop"
              (("Exec=/usr/bin/spice-vdagent")
               (string-append "Exec=" (assoc-ref outputs "out")
                              "/bin/spice-vdagent")))))
         (add-after 'unpack 'fix-test-termination
           (lambda _
             ;; The termination tests depend on finding the socket file name
             ;; in the spice-vdagent command line it launched, but by default
             ;; ps truncates its output, which causes the test to fail (see:
             ;; https://gitlab.freedesktop.org/spice/linux/vd_agent/-/merge_requests/36).
             (substitute* "tests/test-termination.c"
               (("ps -ef")
                "ps -efww")))))))
    (inputs
      (list alsa-lib
            dbus
            glib
            gtk+
            libdrm
            libpciaccess
            libx11
            libxext
            libxfixes
            libxinerama
            libxrandr
            spice-protocol))
    (native-inputs
     (list pkg-config procps))             ;tests use 'ps'
    (synopsis "Spice agent for Linux")
    (description "Spice-vdagent enables sharing the clipboard and guest display
resolution scaling on graphical console window resize.")
    (home-page "https://www.spice-space.org")
    (license license:gpl3+)))

(define-public libcacard
  (package
    (name "libcacard")
    (version "2.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gitlab.freedesktop.org/spice/libcacard/uploads/"
                    "13b249e695a0d9aa7cb501b1a85ebab1"
                    "/libcacard-" version ".tar.xz"))
              (sha256
               (base32
                "1rrjlclm6ad63gah1fa4yfwrz4z6vgq2yrybbvzvvdbxrgl4vgzv"))))
    (build-system meson-build-system)
    (propagated-inputs
     (list glib ; Requires: in the pkg-config file
           nss ; Requires.private: in the pkg-config
           pcsc-lite))       ; file
    (native-inputs
     (list openssl
           `(,nss "bin")
           opensc
           gnutls
           pkg-config
           which))
    (synopsis "Emulate and share smart cards with virtual machines")
    (description
     "The @acronym{CAC,Common Access Card} library can be used to emulate and
share smart cards from client system to local or remote virtual machines.")
    (home-page "https://gitlab.freedesktop.org/spice/libcacard")
    (license license:lgpl2.1+)))

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
      (list gtk+ gtk-vnc libcap libxml2 spice-gtk))
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
