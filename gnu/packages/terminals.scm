;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Mckinley Olsen <mck.olsen@gmail.com>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
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

(define-module (gnu packages terminals)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-26))

(define-public tilda
  (package
    (name "tilda")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/lanoxx/tilda/archive/"
                                  "tilda-" version ".tar.gz"))
              (sha256
               (base32
                "1cc4qbg1m3i04lj5p6i6xbd0zvy1320pxdgmjhz5p3j95ibsbfki"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                 (add-before 'patch-source-shebangs 'autogen
                  (lambda _       ; Avoid running ./configure.
                   (substitute* "autogen.sh"
                                (("^.*\\$srcdir/configure.*") ""))
                   (zero? (system* "sh" "autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libconfuse" ,libconfuse)
       ("vte" ,vte)))
    (synopsis "GTK+-based drop-down terminal")
    (description "Tilda is a terminal emulator similar to normal terminals like
gnome-terminal (GNOME) or Konsole (KDE), with the difference that it drops down
from the edge of a screen when a certain configurable hotkey is pressed.  This
is similar to the built-in consoles in some applications.  Tilda is highly
configurable through a graphical wizard.")
    (home-page "https://github.com/lanoxx/tilda")
    (license license:gpl2+)))

(define-public termite
  (package
    (name "termite")
    (version "13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url (string-append "https://github.com/thestinger/"
                                  name ".git"))
              (commit (string-append "v" version))
              (recursive? #t)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "02cn70ygl93ghhkhs3xdxn5b1yadc255v3yp8cmhhyzsv5027hvj"))))
    (build-system gnu-build-system)
    (arguments
      `(#:phases
        (modify-phases %standard-phases
          (delete 'configure))
        #:tests? #f
        ;; This sets the destination when installing the necessary terminal
        ;; capability data, which are not provided by 'ncurses'.  See
        ;; <https://lists.gnu.org/archive/html/bug-ncurses/2009-10/msg00031.html>.
        #:make-flags (list "PREFIX="
                           (string-append "VERSION=v" (version))
                           (string-append "DESTDIR="
                                          (assoc-ref %outputs "out")))))
    (inputs
     `(("vte", vte-ng)
       ("gtk+", gtk+)
       ("ncurses", ncurses)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))

    ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
    ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
    (native-search-paths
      (list (search-path-specification
              (variable "TERMINFO_DIRS")
              (files '("share/terminfo")))))
    (home-page "https://github.com/thestinger/termite/")
    (synopsis "Keyboard-centric, VTE-based terminal")
    (description "Termite is a minimal terminal emulator designed for use with
tiling window managers.  It is a modal application, similar to Vim, with an
insert mode and command mode where keybindings have different functions.")

    ;; Files under util/ are under the Expat license; the rest is LGPLv2+.
    (license license:lgpl2.0+)))

(define-public asciinema
  (package
    (name "asciinema")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asciinema" version))
       (sha256
        (base32
         "1jrf8c8711gkdilmvyv3d37kp8xfvdc5cqighw5k92a6g9z4acgv"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-exec-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((ncurses (assoc-ref inputs "ncurses")))
               (substitute* "asciinema/recorder.py"
                 (("'tput'")
                  (string-append "'" ncurses "/bin/tput'"))))
             #t)))))
    (inputs `(("ncurses" ,ncurses)))
    (native-inputs
     ;; For tests.
     `(("python-requests" ,python-requests)))
    (home-page "https://asciinema.org")
    (synopsis "Terminal session recorder")
    (description
     "Use asciinema to record and share your terminal sessions, the right way.
Forget screen recording apps and blurry video.  Enjoy a lightweight, purely
text-based approach to terminal recording.")
    (license license:gpl3)))

(define-public libtsm
  (package
    (name "libtsm")
    (version "3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://freedesktop.org/software/kmscon/releases/"
                    "libtsm-" version ".tar.xz"))
              (sha256
               (base32
                "01ygwrsxfii0pngfikgqsb4fxp8n1bbs47l7hck81h9b9bc1ah8i"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libxkbcommon" ,libxkbcommon)))
    (synopsis "Xterm state machine library")
    (description "TSM is a state machine for DEC VT100-VT520 compatible
terminal emulators.  It tries to support all common standards while keeping
compatibility to existing emulators like xterm, gnome-terminal, konsole, etc.")
    (home-page "https://www.freedesktop.org/wiki/Software/libtsm")
    ;; Hash table implementation is lgpl2.1+ licensed.
    ;; The wcwidth implementation in external/wcwidth.{h,c} uses a license
    ;; derived from ISC.
    ;; UCS-4 to UTF-8 encoding is copied from "terminology" which is released
    ;; under the bsd 2 license.
    (license (list license:expat license:lgpl2.1+ license:isc license:bsd-2))))

(define-public kmscon
  (package
    (name "kmscon")
    (version "8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://freedesktop.org/software/kmscon/releases/"
                    "kmscon-" version ".tar.xz"))
              (sha256
               (base32
                "0axfwrp3c8f4gb67ap2sqnkn75idpiw09s35wwn6kgagvhf1rc0a"))
              (modules '((guix build utils)))
              (snippet
               ;; Use elogind instead of systemd.
               '(begin
                  (substitute* "configure"
                    (("libsystemd-daemon libsystemd-login")
                     "libelogind"))
                  (substitute* "src/uterm_systemd.c"
                    (("#include <systemd/sd-login.h>")
                     "#include <elogind/sd-login.h>")
                    ;; We don't have this header.
                    (("#include <systemd/sd-daemon\\.h>")
                     "")
                    ;; Replace the call to 'sd_booted' by the truth value.
                    (("sd_booted\\(\\)")
                     "1"))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libxslt" ,libxslt)                       ;to build the man page
       ("libxml2" ,libxml2)                       ;for XML_CATALOG_FILES
       ("docbook-xsl" ,docbook-xsl)))
    (inputs
     `(("libdrm" ,libdrm)
       ("libtsm" ,libtsm)
       ("libxkbcommon" ,libxkbcommon)
       ("logind" ,elogind)
       ("mesa" ,mesa)
       ("pango" ,pango)
       ("udev" ,eudev)))
    (synopsis "Linux KMS-based terminal emulator")
    (description "Kmscon is a terminal emulator based on Linux's @dfn{kernel
mode setting} (KMS).  It can replace the in-kernel virtual terminal (VT)
implementation with a user-space console.  Compared to the Linux console,
kmscon provides enhanced features including XKB-compatible internationalized
keyboard support, UTF-8 input/font support, hardware-accelerated rendering,
multi-seat support, a replacement for @command{mingetty}, and more.")
    (home-page "https://www.freedesktop.org/wiki/Software/kmscon")
    ;; Hash table implementation is lgpl2.1+ licensed.
    ;; The wcwidth implementation in external/wcwidth.{h,c} uses a license
    ;; derived from ISC.
    ;; UCS-4 to UTF-8 encoding is copied from "terminology" which is released
    ;; under the bsd 2 license.
    ;; Unifont-Font is from http://unifoundry.com/unifont.html and licensed
    ;; under the terms of the GNU GPL.
    (license (list license:expat license:lgpl2.1+ license:bsd-2
                   license:gpl2+))
    (supported-systems (filter (cut string-suffix? "-linux" <>)
                               %supported-systems))))

(define-public libtermkey
  (package
    (name "libtermkey")
    (version "0.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.leonerd.org.uk/code/"
                                  name "/" name "-" version ".tar.gz"))
              (sha256
               (base32 "1xfj6lchhfljmbcl6dz8dpakppyy13nbl4ykxiv5x4dr9b4qf3bc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list
                     "CC=gcc"
                     (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:test-target "test"))
    (inputs `(("ncurses", ncurses)))
    (native-inputs `(("libtool", libtool)
                     ("perl-test-harness" ,perl-test-harness)
                     ("pkg-config", pkg-config)))
    (synopsis "Keyboard entry processing library for terminal-based programs")
    (description
     "Libtermkey handles all the necessary logic to recognise special keys, UTF-8
combining, and so on, with a simple interface.")
    (home-page "http://www.leonerd.org.uk/code/libtermkey")
    (license license:expat)))

(define-public picocom
  (package
    (name "picocom")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/npat-efault/picocom"
                    "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1knl6dglnrynx1fhy21nylw56i1q3dkizkgxzkq42mb7ilah8f9y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("CC=gcc")
       #:tests? #f ; No tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "picocom" bin)
               (install-file "picocom.1" man)))))))
    (home-page "https://github.com/npat-efault/picocom")
    (synopsis "Minimal dumb-terminal emulation program")
    (description "It was designed to serve as a simple, manual, modem
configuration, testing, and debugging tool.  It has also serves well
as a low-tech serial communications program to allow access to all
types of devices that provide serial consoles.")
    (license license:gpl2+)))

(define-public beep
  (package
    (name "beep")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.johnath.com/" name "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bgch6jq5cahakk3kbr9549iysf2dik09afixxy5brbxk1xfzb2r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-makefile
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile" (("/usr") (assoc-ref outputs "out")))
             #t))
         (add-before 'install 'create-output-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref %outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/man/man1"))))))))
    (synopsis "Linux command-line utility to control the PC speaker")
    (description "beep allows the user to control the PC speaker with precision,
allowing different sounds to indicate different events.  While it can be run
quite happily on the command line, its intended place of residence is within
scripts, notifying the user when something interesting occurs.  Of course, it
has no notion of what's interesing, but it's very good at that notifying part.")
    (home-page "http://www.johnath.com/beep")
    (license license:gpl2+)))

(define-public unibilium
  (package
    (name "unibilium")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mauke/unibilium/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n7a0jrlwhn9nnkna76sbnjrr808m0pmzbiwznmp7rhmjl4z2fk2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:test-target "test"
       ;; FIXME: tests require "prove"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("libtool" ,libtool)))
    (home-page "https://github.com/mauke/unibilium")
    (synopsis "Terminfo parsing library")
    (description "Unibilium is a basic C terminfo library.  It doesn't depend
on curses or any other library.  It also doesn't use global variables, so it
should be thread-safe.")
    (license license:lgpl3+)))

(define-public libvterm
  (package
    (name "libvterm")
    (version "0+bzr681")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.leonerd.org.uk/code/libvterm/"
                           "libvterm-" version ".tar.gz"))
       (sha256
        (base32
         "1s56c8p1qz6frkcri0hg4qyydv2wcccj6n2xmz1dwcdqn38ldsmb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("libtool" ,libtool)
       ("perl" ,perl)))
    (home-page "http://www.leonerd.org.uk/code/libvterm/")
    (synopsis "VT220/xterm/ECMA-48 terminal emulator library")
    (description "Libvterm is an abstract C99 library which implements a VT220
or xterm-like terminal emulator.  It doesn't use any particular graphics
toolkit or output system, instead it invokes callback function pointers that
its embedding program should provide it to draw on its behalf.  It avoids
calling @code{malloc} during normal running state, allowing it to be used in
embedded kernel situations.")
    (license license:expat)))

(define-public cool-retro-term
  (let ((commit "e48719fa44e5307df71dbd0fad234f8a6a53f863")
        (revision "1"))
    (package
      (name "cool-retro-term")
      (version (string-append "1.0.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (file-name (string-append name "-" version "-checkout"))
                (uri (git-reference
                      (url (string-append "https://github.com/Swordfish90/" name))
                      (commit commit)
                      (recursive? #t)))
                (sha256
                 (base32 "1sgqbirninkvgwchr35zgn5vzqvsmrf3cp7lqady1xgrawb8lsz3"))
                (patches
                 (search-patches "cool-retro-term-remove-non-free-fonts.patch"
                                 "cool-retro-term-fix-array-size.patch"
                                 "cool-retro-term-dont-check-uninit-member.patch"
                                 "cool-retro-term-memory-leak-1.patch"))
                (modules '((guix build utils)))
                (snippet
                 '(for-each (lambda (font)
                              (delete-file-recursively
                               (string-append "app/qml/fonts/" font))
                              (substitute* '("app/qml/resources.qrc")
                                (((string-append "<file>fonts/" font ".*"))
                                 "")))
                            '(;"1971-ibm-3278"     ; BSD 3-clause
                              "1977-apple2"        ; Non-Free
                              "1977-commodore-pet" ; Non-Free
                              "1979-atari-400-800" ; Non-Free
                              "1982-commodore64"   ; Non-Free
                              "1985-atari-st"      ; ?
                              "1985-ibm-pc-vga"    ; Unclear
                              ;"modern-fixedsys-excelsior" ; Redistributable
                              ;"modern-hermit"     ; SIL
                              ;"modern-inconsolata"; SIL
                              ;"modern-pro-font-win-tweaked" ; X11
                              ;"modern-proggy-tiny"; X11
                              ;"modern-terminus"   ; SIL
                              "modern-monaco"))))) ; Apple non-free
      (build-system gnu-build-system)
      (inputs
       `(("qtbase" ,qtbase)
         ("qtdeclarative" ,qtdeclarative)
         ("qtgraphicaleffects" ,qtgraphicaleffects)
         ("qtquickcontrols" ,qtquickcontrols)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (share (string-append out "/share")))
                 (substitute* '("qmltermwidget/qmltermwidget.pro")
                   (("INSTALL_DIR = \\$\\$\\[QT_INSTALL_QML\\]")
                    (string-append "INSTALL_DIR = " out "/lib/qt5/qml")))
                 (substitute* '("app/app.pro")
                   (("target.path \\+= /usr")
                    (string-append "target.path += " out))
                   (("icon32.path = /usr/share")
                    (string-append "icon32.path = " share))
                   (("icon64.path = /usr/share")
                    (string-append "icon64.path = " share))
                   (("icon128.path = /usr/share")
                    (string-append "icon128.path = " share))
                   (("icon256.path = /usr/share")
                    (string-append "icon256.path = " share)))
                 (zero? (system* "qmake")))))
           (add-before 'install 'fix-Makefiles
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (substitute* '("Makefile")
                   (("\\$\\(INSTALL_ROOT\\)/usr") out)))))
           (add-after 'install 'wrap-executable
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (qml "/lib/qt5/qml"))
                 (wrap-program (string-append out "/bin/cool-retro-term")
                   `("QML2_IMPORT_PATH" ":" prefix
                     (,(string-append out qml)
                      ,(string-append
                        (assoc-ref inputs "qtdeclarative") qml)
                      ,(string-append
                        (assoc-ref inputs "qtgraphicaleffects") qml)
                      ,(string-append
                        (assoc-ref inputs "qtquickcontrols") qml)))))))
           (add-after 'install 'add-alternate-name
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (symlink (string-append bin "/cool-retro-term")
                          (string-append bin "/crt"))))))))
      (synopsis "Terminal emulator")
      (description
       "Cool-retro-term (crt) is a terminal emulator which mimics the look and
feel of the old cathode ray tube (CRT) screens.  It has been designed to be
eye-candy, customizable, and reasonably lightweight.")
      (home-page "https://github.com/Swordfish90/cool-retro-term")
      (license (list
                license:gpl2+           ; qmltermwidget
                license:gpl3+           ; cool-retro-term
                ;; Fonts
                license:silofl1.1
                license:x11
                license:bsd-3)))))

(define-public sakura
  (package
    (name "sakura")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/" name "/trunk/"
                                  version "/+download/" name "-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "0fhcn3540iw22l5zg3njh5z8cj0g2n9p6fvagjqa5zc323jfsc7b"))))
    (build-system cmake-build-system)
    (arguments
     ;; no check phase
     '(#:tests? #f))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("perl" ,perl)               ; for pod2man
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libxft" ,libxft)
       ("vte" ,vte)))
    (home-page "https://launchpad.net/sakura")
    (synopsis "A simple but powerful libvte-based terminal emulator")
    (description "@code{Sakura} is a terminal emulator based on GTK+ and VTE.
It's a terminal emulator with few dependencies, so you don't need a full GNOME
desktop installed to have a decent terminal emulator.")
    (license license:gpl2)))
