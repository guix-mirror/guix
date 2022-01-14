;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Todor Kondić <tk.code@protonmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages vnc)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public remmina
  (package
    (name "remmina")
    (version "1.4.23")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.com/Remmina/Remmina")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j0fiz76z4y08w136vs8igqxxg42hx61r5hf6sylcr0c424sc9rk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:configure-flags
       (list
        ;; Disable online version checking.
        "-DWITH_NEWS=OFF")
       #:imported-modules
       ((guix build glib-or-gtk-build-system)
        ,@%cmake-build-system-modules)
       #:modules
       (((guix build glib-or-gtk-build-system)
         #:prefix glib-or-gtk:)
        (guix build cmake-build-system)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'glib-or-gtk-wrap 'wrap-typelibs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (name)
                  (let ((file (string-append out "/bin/" name))
                        (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                    (wrap-program file
                      `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))
                '("remmina" "remmina-file-wrapper"))))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           `(,gtk+ "bin")
           intltool
           pkg-config))
    (inputs
     (list libappindicator
           atk
           avahi
           cairo
           cups
           ffmpeg
           freerdp                      ; for rdp plugin
           libgcrypt
           librsvg
           glib
           gnome-keyring
           gsettings-desktop-schemas
           gtk+
           harfbuzz
           json-glib
           libsecret                    ; for secret plugin
           libsoup-minimal-2
           libssh                       ; for ssh plugin
           libvnc                       ; for vnc plugin
           openssl
           pango
           pcre2                        ; for exec plugin
           shared-mime-info
           libsodium
           spice-gtk                    ; for spice plugin
           telepathy-glib
           vte                          ; for st plugin
           wayland
           webkitgtk                    ; for www plugin
           libx11
           libxext                      ; for xdmcp plugin
           xdg-utils
           libxkbfile))                 ; for nx plugin
    (propagated-inputs
     (list dconf))
    (home-page "https://remmina.org/")
    (synopsis "Remote Desktop Client")
    (description "Remmina is a client to use other desktops remotely.
RDP, VNC, SPICE, NX, XDMCP, SSH and EXEC network protocols are supported.")
    (license license:gpl2+)))

(define-public tigervnc-client
  ;; xorg-server 21 support was merged 2 weeks after the last (1.12.0) release.
  (let ((revision "0")
        (commit "b484c229853a08c7f254a4c6efbaf3c9e85b5074"))
    (package
      (name "tigervnc-client")
      (version (git-version "1.12.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/TigerVNC/tigervnc")
               (commit commit)))
         (sha256
          (base32 "125dnn05ps7vfsxlxmzm05w99lhm8hk8j4hpxl1mlzb5j0hp1061"))
         (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f                 ; Tests that do exists are not automated.
                  #:phases (modify-phases %standard-phases
                             (replace 'install
                               (lambda* (#:key outputs #:allow-other-keys)
                                 (with-directory-excursion "vncviewer"
                                   (invoke "make" "install")))))))
      (native-inputs
       (list autoconf gettext-minimal automake))
      (inputs
       (list zlib
             gnutls
             libjpeg-turbo
             fltk
             linux-pam
             libx11
             libxext
             libxtst
             libxrandr
             libxdamage
             pixman))
      (home-page "https://tigervnc.org/")
      (synopsis "High-performance, platform-neutral
implementation of VNC (client)")
      (description "TigerVNC is a client/server implementation of VNC (Virtual
Network Computing).  It provides enough performance to run even 3D and video
applications.  It also provides extensions for advanced authentication methods
and TLS encryption.  This package installs only the VNC client, the
application which is needed to connect to VNC servers.")
      (license license:gpl2))))

;; A VNC server is, in fact, an X server so it seems like a good idea
;; to build on the work already done for xorg-server package.  This is
;; not entirely compatible with the recommendation in BUILDING.txt
;; where the client is built first, then the source code of the X
;; server is copied into a subdir of the build directory, patched with
;; VNC additions and then build and installed as Xvnc.  The procedure
;; was turned around, where TigerVNC code is downloaded and built
;; inside the Guix X server build dir. Also, the VNC patching process
;; for the X server is automated in a straightforward manner.
(define-public tigervnc-server
  (package
    (inherit xorg-server)
    (name "tigervnc-server")
    (version (package-version tigervnc-client))
    (native-inputs
     `(("tigervnc-src" ,(package-source tigervnc-client))
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("gettext-minimal" ,gettext-minimal)
       ("font-util" ,font-util)
       ("cmake" ,cmake)
       ("perl" ,perl)
       ,@(package-native-inputs tigervnc-client)
       ,@(package-inputs tigervnc-client)
       ,@(package-native-inputs xorg-server)))
    (inputs
     (modify-inputs (package-inputs xorg-server)
       (prepend perl coreutils xauth)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs xorg-server)
       (prepend xauth)))
    (arguments
     (substitute-keyword-arguments
         (package-arguments xorg-server)
       ((#:configure-flags flags)
        `(append '("--with-pic"         ; Taken from BUILDING.txt
                   "--without-dtrace"
                   "--disable-static"
                   "--disable-dri2"
                   "--disable-xinerama"
                   "--disable-xvfb"
                   "--disable-xnest"
                   "--disable-xorg"
                   "--disable-dmx"
                   "--disable-xwin"
                   "--disable-xephyr"
                   "--disable-kdrive"
                   ;; "--disable-config-dbus" ; This was a warning.
                   "--disable-config-hal"
                   "--disable-config-udev"
                   "--disable-dri2"
                   ;; "--enable-install-libxf86config" ; This, too, was a warning.
                   "--enable-glx")
                 (delete "--enable-xephyr" ,flags)))
       ((#:modules modules)
        `(append '((ice-9 ftw)
                   (ice-9 match)
                   (guix build utils)
                   (guix build gnu-build-system))
                 modules))
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'check)              ;)
           (add-after 'unpack 'copy-tvnc-xserver
             (lambda _
               (let*
                   ((tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                    (tvnc-xserver (string-append tvnc-src "/unix/xserver")))
                 (copy-recursively tvnc-xserver "."))))
           (add-after 'copy-tvnc-xserver 'patch-xserver
             (lambda _
               (invoke "patch" "-p1" "-i"
                       (string-append (assoc-ref %build-inputs "tigervnc-src")
                                      "/unix/xserver21.1.1.patch"))
               (invoke "autoreconf" "-fiv")))
           (add-before 'build 'build-tigervnc
             (lambda _
               (let* ((out (assoc-ref %outputs "out"))
                      (tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                      (tvnc-build (string-append (getcwd) "/tigervnc-build")))
                 (mkdir-p tvnc-build)
                 (with-directory-excursion tvnc-build
                   (invoke "cmake" "-G" "Unix Makefiles"
                           (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                           tvnc-src)
                   (invoke "make" "-j" (number->string (parallel-job-count)))))))
           (replace 'build
             (lambda _
               (let*  ((tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                       (tvnc-build (string-append (getcwd) "/tigervnc-build"))
                       (srcarg (string-append "TIGERVNC_SRCDIR=" tvnc-src))
                       (buildarg (string-append "TIGERVNC_BUILDDIR=" tvnc-build)))
                 (invoke "make" srcarg buildarg "-j"
                         (number->string (parallel-job-count))))))
           (add-before 'install 'install-tigervnc-aux
             (lambda _
               (let*  ((out (assoc-ref %outputs 'out))
                       (tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                       (tvnc-build (string-append (getcwd) "/tigervnc-build"))
                       (srcarg (string-append "TIGERVNC_SRCDIR=" tvnc-src))
                       (buildarg (string-append "TIGERVNC_BUILDDIR=" tvnc-build)))
                 (with-directory-excursion (string-append tvnc-build "/unix")
                   (invoke "make" srcarg buildarg "install")))))
           (replace 'install
             (lambda* _
               (let*  ((tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                       (tvnc-build (string-append (getcwd) "/tigervnc-build"))
                       (srcarg (string-append "TIGERVNC_SRCDIR=" tvnc-src))
                       (buildarg (string-append "TIGERVNC_BUILDDIR=" tvnc-build)))
                 (invoke "make" "install" srcarg buildarg))))))))
    (description "TigerVNC is a client/server implementation of VNC (Virtual
Network Computing).  It provides enough performance to run even 3D and video
applications.  It also provides extensions for advanced authentication methods
and TLS encryption.  This package installs the VNC server, a program that will
enable users with VNC clients to log into a graphical session on the machine
where the server is installed.")))

(define-public libvnc
  (package
    (name "libvnc")
    (version "0.9.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LibVNC/libvncserver")
             (commit (string-append "LibVNCServer-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zz0hslw8b1p3crnfy3xnmrljik359h83dpk64s697dqdcrzy141"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-cc-reference
                    (lambda _
                      (substitute* "test/includetest.sh"
                        (("^cc -I")
                         "gcc -I"))
                      #t)))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("lzo" ,lzo)
       ("sdl2" ,sdl2)))
    (home-page "https://libvnc.github.io/")
    (synopsis "Cross-platform C libraries for implementing VNC server or
client")
    (description "This package provides @code{LibVNCServer} and
@code{LibVNCClient}.  These are cross-platform C libraries that allow you to
easily implement VNC server or client functionality in your program.")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))
