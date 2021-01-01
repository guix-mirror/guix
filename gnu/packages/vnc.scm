;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Todor Kondić <tk.code@protonmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xorg))

(define-public tigervnc-client
  (let ((commit "920d9c4d6562ecabf79497bc901d50522d4bc661"))
    (package
      (name "tigervnc-client")
      (version (git-version "1.10.1" "1" commit))
      (source (origin
                (method git-fetch)
                (uri
                 (git-reference
                  (url "https://github.com/TigerVNC/tigervnc")
                  (commit commit)))
                (sha256
                 (base32
                  "1lp6mxl5dqlkrzx0q145jzgpbwvhsni3fj6x9ngf8v5s63x82q1p"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       '(#:tests? #f ; Tests that do exists are not automated.
         #:phases (modify-phases %standard-phases
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (with-directory-excursion "vncviewer"
                          (invoke "make" "install")))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("gettext-minimal" ,gettext-minimal)
         ("automake" ,automake)))
      (inputs
       `(("zlib" ,zlib)
         ("gnutls" ,gnutls)
         ("libjpeg-turbo" ,libjpeg-turbo)
         ("fltk" ,fltk)
         ("linux-pam" ,linux-pam)
         ("libx11" ,libx11)
         ("libxext" ,libxext)
         ("libxtst" ,libxtst)
         ("libxrandr" ,libxrandr)
         ("libxdamage" ,libxdamage)))
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
       ("gcc-toolchain" ,gcc-toolchain)
       ("perl" ,perl)
       ,@(package-native-inputs tigervnc-client)
       ,@(package-inputs tigervnc-client)
       ,@(package-native-inputs xorg-server)))
    (inputs
     `(("perl" ,perl)
       ("coreutils" ,coreutils)
       ("xauth" ,xauth)
       ,@(package-inputs xorg-server)))
    (propagated-inputs
     `(("xauth" ,xauth)
       ,@(package-propagated-inputs xorg-server)))
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
                 (copy-recursively tvnc-xserver ".")
                 #t)))
           (add-after 'copy-tvnc-xserver 'patch-xserver
             (lambda _
               (let*
                   ((tvnc-src (assoc-ref %build-inputs "tigervnc-src"))
                    (xorg-server-version ,(package-version xorg-server))
                    (which-patch (lambda ()
                                   (let*
                                       ((patch-num (apply string-append
                                                          (list-head (string-split xorg-server-version
                                                                                   #\.)
                                                                     2)))
                                        (fn (format #f "~a/unix/xserver~a.patch" tvnc-src patch-num)))
                                     (when (not (file-exists? fn))
                                       (error (format #f "Patch file, ~a,
corresponding to the input xorg-server version, does not exist.  Installation
will fail.  " fn)))

                                     fn))) ; VNC patches for xserver have the
                                           ; form xserverXY[Y].patch, where
                                           ; X.Y[Y].Z is the Xorg server
					; version.
                    (xserver-patch (which-patch)))
                 (invoke "patch" "-p1" "-i" xserver-patch)
                 (invoke "autoreconf" "-fiv"))))
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
     `(("pkg-config" ,pkg-config)))
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
