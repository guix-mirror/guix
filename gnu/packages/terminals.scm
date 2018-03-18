;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Mckinley Olsen <mck.olsen@gmail.com>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
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
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
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
  #:use-module (gnu packages textutils)
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
     `(("vte" ,vte-ng)
       ("gtk+" ,gtk+)
       ("ncurses" ,ncurses)))
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
    (inputs `(("ncurses" ,ncurses)))
    (native-inputs `(("libtool" ,libtool)
                     ("perl-test-harness" ,perl-test-harness)
                     ("pkg-config" ,pkg-config)))
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
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mauke/unibilium/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hbf011d8nzsp7c96fidjiq8yw8zlxf6f1s050ii2yyampvb8ib0"))))
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

(define-public go-github.com-nsf-termbox-go
  (let ((commit "4ed959e0540971545eddb8c75514973d670cf739")
        (revision "0"))
    (package
      (name "go-github.com-nsf-termbox-go")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nsf/termbox-go.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vx64i1mg660if3wwm81p4b7lzxfb3qbr39i7misdyld3fc486p9"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/nsf/termbox-go"))
      (propagated-inputs
       `(("go-github.com-mattn-go-runewidth"
          ,go-github.com-mattn-go-runewidth)))
      (synopsis "@code{termbox} provides a minimal API for text-based user
interfaces")
      (description
       "Termbox is a library that provides a minimalistic API which allows the
programmer to write text-based user interfaces.")
      (home-page "https://github.com/nsf/termbox-go")
      (license license:expat))))

(define-public go-golang.org-x-crypto-ssh-terminal
  (let ((commit "c78caca803c95773f48a844d3dcab04b9bc4d6dd")
        (revision "0"))
    (package
      (name "go-golang.org-x-crypto-ssh-terminal")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/crypto")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0vxlfxr9y681yn2cfh6dbqmq35vvq4f45ay0mm31ffkny9cms0y4"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "golang.org/x/crypto/ssh/terminal"
         #:unpack-path "golang.org/x/crypto"
         #:phases
         (modify-phases %standard-phases
           (add-before 'reset-gzip-timestamps 'make-gzip-archive-writable
             (lambda* (#:key outputs #:allow-other-keys)
               (map (lambda (file)
                      (make-file-writable file))
                    (find-files
                     (string-append (assoc-ref outputs "out")
                                    "/src/golang.org/x/crypto/ed25519/testdata")
                     ".*\\.gz$"))
               #t)))))
      (synopsis "Support functions for dealing with terminals in Go")
      (description "@code{terminal} provides support functions for dealing
with terminals in Go.")
      (home-page "https://go.googlesource.com/crypto/")
      (license license:bsd-3))))

(define-public go-github.com-howeyc-gopass
  (let ((commit "bf9dde6d0d2c004a008c27aaee91170c786f6db8")
        (revision "0"))
    (package
      (name "go-github.com-howeyc-gopass")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/howeyc/gopass.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jxzyfnqi0h1fzlsvlkn10bncic803bfhslyijcxk55mgh297g45"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/howeyc/gopass"))
      (propagated-inputs
       `(("go-golang.org-x-crypto-ssh-terminal"
          ,go-golang.org-x-crypto-ssh-terminal)))
      (synopsis "Retrieve password from a terminal or piped input in Go")
      (description
       "@code{gopass} is a Go package for retrieving a password from user
terminal or piped input.")
      (home-page "https://github.com/howeyc/gopass")
      (license license:isc))))

(define-public python-pyte
  (package
    (name "python-pyte")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyte" version))
       (sha256
        (base32
         "1an54hvyjm8gncx8cgabz9mkpgjkdb0bkyjlkh7g7f94nr3wnfl7"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-failing-test
           ;; TODO: Reenable when the `captured` files required by this test
           ;; are included in the archive.
           (lambda _
             (delete-file "tests/test_input_output.py")
             #t)))))
    (propagated-inputs
     `(("python-wcwidth" ,python-wcwidth)))
    (native-inputs
     `(("python-pytest-runner" ,python-pytest-runner)
       ("python-pytest" ,python-pytest)))
    (home-page "https://pyte.readthedocs.io/")
    (synopsis "Simple VTXXX-compatible terminal emulator")
    (description "@code{pyte} is an in-memory VTxxx-compatible terminal
emulator.  @var{VTxxx} stands for a series of video terminals, developed by
DEC between 1970 and 1995.  The first and probably most famous one was the
VT100 terminal, which is now a de-facto standard for all virtual terminal
emulators.

pyte is a fork of vt102, which was an incomplete pure Python implementation
of VT100 terminal.")
    (license license:lgpl3+)))

(define-public python2-pyte
  (package-with-python2 python-pyte))

(define-public python-blessings
  (package
    (name "python-blessings")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "blessings" version))
       (sha256
        (base32
         "1smngy65p8mi62lgm04icasx22v976szhs2aq95y2ljmi1srb4bl"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: For py3, 2to2 is used to convert the code, but test-suite fails
     `(#:tests? #f))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/erikrose/blessings")
    (synopsis "Python module to manage terminal color, styling, and
positioning")
    (description "Blessings is a pythonic API to manipulate terminal color,
styling, and positioning.  It provides similar features to curses but avoids
some of curses’s limitations: it does not require clearing the whole screen
for little changes, provides a scroll-back buffer after the program exits, and
avoids styling altogether when the output is redirected to something other
than a terminal.")
    (license license:expat)))

(define-public python2-blessings
  (package-with-python2 python-blessings))

(define-public python-curtsies
  (package
    (name "python-curtsies")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "curtsies" version))
       (sha256
        (base32
         "1vljmw3sy6lrqahhpyg4gk13mzcx3mwhvg8s41698ms3cpgkjipc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "nosetests" "-v"))))))
    (propagated-inputs
     `(("python-blessings" ,python-blessings)
       ("python-wcwidth" ,python-wcwidth)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pyte" ,python-pyte)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/thomasballinger/curtsies")
    (synopsis "Library for curses-like terminal interaction with colored
strings")
    (description "Curtsies is a Python library for interacting with the
terminal.  It features string-like objects which carry formatting information,
per-line fullscreen terminal rendering, and keyboard input event reporting.")
    (license license:expat)))

(define-public python2-curtsies
  (package-with-python2 python-curtsies))
