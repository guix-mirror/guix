;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (gnu packages wine)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages image)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public wine
  (package
    (name "wine")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.winehq.org/wine/source/2.0"
                                  "/wine-" version ".tar.xz"))
              (sha256
               (base32
                "0nlq6apyq7hq36l3g6gw76lhi8ijz11v3v8m4vxy8d6x1qsppq5m"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("gettext" ,gettext-minimal)
                     ("flex" ,flex)
                     ("bison" ,bison)
                     ("perl" ,perl)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("cups" ,cups)
       ("eudev" ,eudev)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glu" ,glu)
       ("gnutls" ,gnutls)
       ("gst-plugins-base" ,gst-plugins-base)
       ("lcms" ,lcms)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libgphoto2" ,libgphoto2)
       ("libmpg123" ,mpg123)
       ("libldap" ,openldap)
       ("libnetapi" ,samba)
       ("libsane" ,sane-backends)
       ("libpcap" ,libpcap)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)
       ("libtiff" ,libtiff)
       ("libICE" ,libice)
       ("libX11" ,libx11)
       ("libXi" ,libxi)
       ("libXext" ,libxext)
       ("libXcursor" ,libxcursor)
       ("libXrender" ,libxrender)
       ("libXrandr" ,libxrandr)
       ("libXinerama" ,libxinerama)
       ("libXxf86vm" ,libxxf86vm)
       ("libXcomposite" ,libxcomposite)
       ("ncurses" ,ncurses)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("unixodbc" ,unixodbc)
       ("v4l-utils" ,v4l-utils)
       ("zlib" ,zlib)))
    (arguments
     `(;; Force a 32-bit build targeting a similar architecture, i.e.:
       ;; armhf for armhf/aarch64, i686 for i686/x86_64.
       #:system ,@(match (%current-system)
                    ((or "armhf-linux" "aarch64-linux")
                     `("armhf-linux"))
                    (_
                     `("i686-linux")))

       ;; XXX: There's a test suite, but it's unclear whether it's supposed to
       ;; pass.
       #:tests? #f

       #:configure-flags
       (list (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib/wine32"))

       #:make-flags
       (list "SHELL=bash"
             (string-append "libdir=" %output "/lib/wine32"))

       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-dlopen-paths
           ;; Hardcode dlopened sonames to absolute paths.
           (lambda _
             (let* ((library-path (search-path-as-string->list
                                   (getenv "LIBRARY_PATH")))
                    (find-so (lambda (soname)
                               (search-path library-path soname))))
               (substitute* "include/config.h"
                 (("(#define SONAME_.* )\"(.*)\"" _ defso soname)
                  (format #f "~a\"~a\"" defso (find-so soname))))
               #t))))))
    (home-page "https://www.winehq.org/")
    (synopsis "Implementation of the Windows API (32-bit only)")
    (description
     "Wine (originally an acronym for \"Wine Is Not an Emulator\") is a
compatibility layer capable of running Windows applications.  Instead of
simulating internal Windows logic like a virtual machine or emulator, Wine
translates Windows API calls into POSIX calls on-the-fly, eliminating the
performance and memory penalties of other methods and allowing you to cleanly
integrate Windows applications into your desktop.")
    (license license:lgpl2.1+)

    ;; It really only supports IA32, but building on x86_64 will have the same
    ;; effect as building on i686 anyway.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define-public wine64
  (package
    (inherit wine)
    (name "wine64")
    (inputs `(("wine" ,wine)
              ,@(package-inputs wine)))
    (arguments
     `(#:make-flags
       (list "SHELL=bash"
             (string-append "libdir=" %output "/lib/wine64"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'copy-wine32-binaries
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((wine32 (assoc-ref %build-inputs "wine"))
                    (out (assoc-ref %outputs "out")))
               ;; Copy the 32-bit binaries needed for WoW64.
               (copy-file (string-append wine32 "/bin/wine")
                          (string-append out "/bin/wine"))
               (copy-file (string-append wine32 "/bin/wine-preloader")
                          (string-append out "/bin/wine-preloader"))
               #t)))
         (add-after 'compress-documentation 'copy-wine32-manpage
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((wine32 (assoc-ref %build-inputs "wine"))
                    (out (assoc-ref %outputs "out")))
               ;; Copy the missing man file for the wine binary from wine.
               (copy-file (string-append wine32 "/share/man/man1/wine.1.gz")
                          (string-append out "/share/man/man1/wine.1.gz"))
               #t)))
         (add-after 'configure 'patch-dlopen-paths
           ;; Hardcode dlopened sonames to absolute paths.
           (lambda _
             (let* ((library-path (search-path-as-string->list
                                   (getenv "LIBRARY_PATH")))
                    (find-so (lambda (soname)
                               (search-path library-path soname))))
               (substitute* "include/config.h"
                 (("(#define SONAME_.* )\"(.*)\"" _ defso soname)
                  (format #f "~a\"~a\"" defso (find-so soname))))
               #t))))
       #:configure-flags
       (list "--enable-win64"
             (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib/wine64"))
       ,@(strip-keyword-arguments '(#:configure-flags #:make-flags #:phases
                                    #:system)
                                  (package-arguments wine))))
    (synopsis "Implementation of the Windows API (WoW64 version)")
    (supported-systems '("x86_64-linux" "aarch64-linux"))))

;; TODO: This is wine development version, provided for historical reasons.
;; We can remove it as soon as a new stable release is out.
(define-public wine-next
  (package (inherit wine)
    (name "wine-next")
    (version "2.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dl.winehq.org/wine/source/2.x"
                                  "/wine-" version ".tar.xz"))
              (sha256
               (base32
                "0g6cwjyqwc660w33453aklh3hpc0b8rrb88dryn23ah6wannvagg"))))))

(define-public wine-staging
  (package
    (inherit wine)
    (name "wine-staging")
    (version "2.21")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/wine-compholio/wine-patched/archive/"
                    "staging-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pjaxj7h3q6y356np908fvsx0bf7yx5crqvgl4hza6gfssdmsr5r"))))
    (inputs `(("gtk+", gtk+)
              ("libva", libva)
              ,@(package-inputs wine)))
    (synopsis "Implementation of the Windows API (staging branch, 32-bit only)")
    (description "Wine-Staging is the testing area of Wine.  It
contains bug fixes and features, which have not been integrated into
the development branch yet.  The idea of Wine-Staging is to provide
experimental features faster to end users and to give developers the
possibility to discuss and improve their patches before they are
integrated into the main branch.")
    (home-page "https://wine-staging.com")
    ;; In addition to the regular Wine license (lgpl2.1+), Wine-Staging
    ;; provides Liberation and WenQuanYi Micro Hei fonts.  Those use
    ;; different licenses.  In particular, the latter is licensed under
    ;; both GPL3+ and Apache 2 License.
    (license
     (list license:lgpl2.1+ license:silofl1.1 license:gpl3+ license:asl2.0))))

(define-public wine64-staging
  (package
    (inherit wine-staging)
    (name "wine64-staging")
    (inputs `(("wine-staging" ,wine-staging)
              ,@(package-inputs wine-staging)))
    (arguments
     `(#:make-flags
       (list "SHELL=bash"
             (string-append "libdir=" %output "/lib/wine64"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'copy-wine32-binaries
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((wine32 (assoc-ref %build-inputs "wine"))
                    (out (assoc-ref %outputs "out")))
               ;; Copy the 32-bit binaries needed for WoW64.
               (copy-file (string-append wine32 "/bin/wine")
                          (string-append out "/bin/wine"))
               (copy-file (string-append wine32 "/bin/wine-preloader")
                          (string-append out "/bin/wine-preloader"))
               #t)))
         (add-after 'compress-documentation 'copy-wine32-manpage
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((wine32 (assoc-ref %build-inputs "wine"))
                    (out (assoc-ref %outputs "out")))
               ;; Copy the missing man file for the wine binary from
               ;; wine-staging.
               (copy-file (string-append wine32 "/share/man/man1/wine.1.gz")
                          (string-append out "/share/man/man1/wine.1.gz"))
               #t)))
         (add-after 'configure 'patch-dlopen-paths
           ;; Hardcode dlopened sonames to absolute paths.
           (lambda _
             (let* ((library-path (search-path-as-string->list
                                   (getenv "LIBRARY_PATH")))
                    (find-so (lambda (soname)
                               (search-path library-path soname))))
               (substitute* "include/config.h"
                 (("(#define SONAME_.* )\"(.*)\"" _ defso soname)
                  (format #f "~a\"~a\"" defso (find-so soname))))
               #t))))
       #:configure-flags
       (list "--enable-win64"
             (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib/wine64"))
       ,@(strip-keyword-arguments '(#:configure-flags #:make-flags #:phases
                                    #:system)
                                  (package-arguments wine-staging))))
    (synopsis "Implementation of the Windows API (staging branch, WoW64
version)")
    (supported-systems '("x86_64-linux" "aarch64-linux"))))
