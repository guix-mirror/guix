;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Andrew Miloradovsky <andrew@interpretmath.pw>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages accessibility)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages base)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages music)
  #:use-module (gnu packages language)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages java)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb))

(define-public libbraille
  (package
    (name "libbraille")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/" name "/files/" name "/"
                       name "-" version "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "05g8r0ypazqn10i7k48iibs8bzc3scdfcnhcykab8j16lhzd27d0"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "bin"))
    (arguments
     `(#:tests? #f                      ; Tests require drivers
       #:configure-flags
       (list
        "--disable-static"
        "--enable-fake")))
    (native-inputs
     (list latex2html pkg-config python-wrapper swig))
    (inputs
     (list glib gtk+-2 libusb-compat))
    (synopsis "Portable Braille Library")
    (description "Libbraille is a library to easily access Braille displays and
terminals.")
    (home-page "https://libbraille.org")
    (license license:lgpl2.1+)))

(define-public brltty
  (package
    (name "brltty")
    (version "6.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://brltty.app/archive/brltty-" version ".tar.gz"))
       (sha256
        (base32 "0zybi9i9izv25g0wphl0snddrhb6xl5879y4pkpjpnxq61wm9gry"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f                      ; No target

       ;; High parallelism may cause errors such as:
       ;;  ranlib: ./libbrlapi_stubs.a: error reading brlapi_stubs.o: file truncated
       #:parallel-build? #f

       #:configure-flags
       (list
        (string-append "--with-libbraille="
                       (assoc-ref %build-inputs "libbraille"))
        (string-append "--with-espeak_ng="
                       (assoc-ref %build-inputs "espeak-ng"))
        (string-append "--with-espeak="
                       (assoc-ref %build-inputs "espeak"))
        (string-append "--with-flite="
                       (assoc-ref %build-inputs "flite"))
        ;; Required for RUNPATH validation.
        (string-append "LDFLAGS=-Wl,-rpath="
                       (assoc-ref %outputs "out")
                       "/lib"))
       #:make-flags
       (list
        (string-append "JAVA_JAR_DIR="
                       (assoc-ref %outputs "out"))
        (string-append "JAVA_JNI_DIR="
                       (assoc-ref %outputs "out"))
        (string-append "OCAML_DESTDIR="
                       (assoc-ref %outputs "out")
                       "/lib")
        (string-append "PYTHON_PREFIX="
                       (assoc-ref %outputs "out"))
        "PYTHON_ROOT=/"
        (string-append "TCL_DIR="
                       (assoc-ref %outputs "out")
                       "/lib")
        "INSTALL_WRITABLE_DIRECTORY=no-thanks")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-errors
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "configure"
               (("/sbin/ldconfig")
                (which "true")))
             ;; Make Python bindings use rpath.
             (substitute* "Bindings/Python/setup.py.in"
               (("extra_compile_args =")
                (string-append "extra_link_args = ['-Wl,-rpath="
                               (assoc-ref outputs "out")
                               "/lib'], "
                               "extra_compile_args = "))))))))
    (native-inputs
     (list clisp
           python-cython
           doxygen
           gettext-minimal
           `(,icedtea "jdk")
           ;; ("linuxdoc" ,linuxdoc-tools)
           ocaml
           ocaml-findlib
           pkg-config
           python-wrapper
           tcl))
    (inputs
     (list alsa-lib
           at-spi2-core
           bluez
           dbus
           espeak
           espeak-ng
           expat
           festival
           flite
           glib
           gpm
           libiconv
           icu4c
           libbraille
           pcre2
           liblouis
           ncurses
           polkit
           speech-dispatcher
           util-linux
           `(,util-linux "lib")
           libx11
           libxaw
           libxaw3d
           libxext
           libxfixes
           libxt
           libxtst))
    (synopsis "Braille TTY")
    (description "BRLTTY is a background process (daemon) which provides access
to the Linux/Unix console (when in text mode) for a blind person using a
refreshable braille display.  It drives the braille display, and provides
complete screen review functionality.  Some speech capability has also been
incorporated.")
    (home-page "https://brltty.app/")
    (license license:lgpl2.1+)))

(define-public florence
  (package
    (name "florence")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/florence/florence/" version
                           "/florence-" version ".tar.bz2"))
       (sha256
        (base32
         "07h9qm22krlwayhzvc391lr23vicw81s48g7rirvx1fj0zyr4aa2"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list "--with-xtst"
                               "--without-docs"
                               "--with-notification")))
    (inputs
     (list libxml2
           libglade
           librsvg
           gstreamer
           cairo
           gtk+
           libxtst
           libxcomposite
           libnotify))
    (native-inputs
     (list gettext-minimal intltool pkg-config))
    (home-page "http://florence.sourceforge.net/")
    (synopsis "Extensible, scalable virtual keyboard for X11")
    (description
     "Florence is an extensible scalable virtual keyboard for X11.
It is useful for people who can't use a real hardware keyboard (for
example for people with disabilities), but you must be able to use
a pointing device (as a mouse, a trackball, a touchscreen or opengazer).

Florence stays out of your way when you don't need it: it appears on the
screen only when you need it.  A timer-based auto-click input method is
available to help to click.")
    ;; The documentation is under FDL1.2, but we do not install the
    ;; documentation.
    (license license:gpl2+)))

(define-public footswitch
  (let ((commit "ca43d53fc2002520cc825d119702afc124303e73")
        (revision "2"))
    (package
      (name "footswitch")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rgerganov/footswitch")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "14pyzc4ws1mj859xs9n4x83wzxxvd3bh5bdxzr6nv267xwx1mq68"))))
      (build-system gnu-build-system)
      (native-inputs
       (list pkg-config))
      (inputs
       (list hidapi))
      (arguments
       `(#:tests? #f ; no tests
         #:make-flags (list (string-append "CC=" ,(cc-for-target)))
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    ;; Install target in the Makefile does not work for Guix
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((bin (string-append (assoc-ref outputs "out")
                                                  "/bin")))
                          (install-file "footswitch" bin)
                          (install-file "scythe" bin)
                          #t))))))
      (home-page "https://github.com/rgerganov/footswitch")
      (synopsis "Command line utility for PCsensor foot switch")
      (description
       "Command line utility for programming foot switches sold by PCsensor.
It works for both single pedal devices and three pedal devices.  All supported
devices have vendorId:productId = 0c45:7403 or 0c45:7404.")
    (license license:expat))))

(define-public xmagnify
  (package
    (name "xmagnify")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/amiloradovsky/magnify.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ngnp5f5zl3v35vhbdyjpymy6mwrs0476fm5nd7dzkba7n841jdh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; none included
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list libx11))
    (home-page "https://gitlab.com/amiloradovsky/magnify")
    (synopsis "Tiny screen magnifier for X11")
    (description
     "This program magnifies a screen region by an integer positive factor and
draws the result on a window.  It is useful as an accessibility tool, which
works with every X Window System based GUI (depends only on libX11); or as an
assistant for graphic designers, who need to select individual pixels.")
    ;; Licensed either under Expat or GPLv2+.
    (license (list license:expat license:gpl2+))))
