;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Mckinley Olsen <mck.olsen@gmail.com>
;;; Copyright © 2016, 2017, 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Valentin Ignatev <valentignatev@gmail.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
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
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages build-tools)   ;for meson-0.55
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-26))

(define-public tilda
  (package
    (name "tilda")
    (version "1.5.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lanoxx/tilda")
                    (commit (string-append "tilda-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0q2i9ny8sh7zjzgvkx8vcvk593wcvchjc4xq4nrlqdd377r7cg5q"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-po-writable
           (lambda _
             (for-each make-file-writable (find-files "po" "."))
             #t)))))
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
    (version "15")
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
          "0hp1x6lj098m3jgna274wv5dv60lnzg22297di68g4hw9djjyd2k"))))
    (build-system gnu-build-system)
    (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'patch-xdg-open
            (lambda _
              (substitute* "termite.cc"
                (("xdg-open") (which "xdg-open")))
              #t))
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
       ("xdg-utils" ,xdg-utils)
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
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asciinema/asciinema")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1a2pysxnp6icyd08mgf66xr6f6j0irnfxdpf3fmzcz31ix7l9kc4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-exec-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((ncurses (assoc-ref inputs "ncurses")))
               (substitute* "asciinema/term.py"
                 (("'tput'")
                  (string-append "'" ncurses "/bin/tput'"))))
             #t))
         (replace 'check
           (lambda _ (invoke "nosetests" "-v"))))))
    (inputs `(("ncurses" ,ncurses)))
    (native-inputs
     ;; For tests.
     `(("python-nose" ,python-nose)))
    (home-page "https://asciinema.org")
    (synopsis "Terminal session recorder")
    (description
     "Use asciinema to record and share your terminal sessions, the right way.
Forget screen recording apps and blurry video.  Enjoy a lightweight, purely
text-based approach to terminal recording.")
    (license license:gpl3)))

(define-public libtsm
  (let ((commit "f70e37982f382b03c6939dac3d5f814450bda253")
        (revision "1"))
    (package
      (name "libtsm")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         ;; The freedesktop repository is no longer maintained.
         (uri (git-reference
               (url (string-append "https://github.com/Aetf/" name))
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0mwn91i5h5d518i1s05y7hzv6bc13vzcvxszpfh77473iwg4wprx"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Remove a bundled copy of libxkbcommon's xkbcommon-keysyms.h.
             (delete-file-recursively "external/xkbcommon")
             #t))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DBUILD_TESTING=ON")))
      (native-inputs
       `(("check" ,check)
         ("libxkbcommon" ,libxkbcommon) ; for xkbcommon-keysyms.h
         ("pkg-config" ,pkg-config)))
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
      (license (list license:expat license:lgpl2.1+ license:isc license:bsd-2)))))

(define-public kmscon
  (let ((commit "01dd0a231e2125a40ceba5f59fd945ff29bf2cdc")
        (revision "1"))
    (package
      (name "kmscon")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                ;; The freedesktop repository is no longer maintained.
                (uri (git-reference
                      (url (string-append "https://github.com/Aetf/" name))
                      (commit commit)))
                (sha256
                 (base32
                  "0q62kjsvy2iwy8adfiygx2bfwlh83rphgxbis95ycspqidg9py87"))
                (patches
                 (search-patches "kmscon-runtime-keymap-switch.patch"))
                (modules '((guix build utils)))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(;; The closure of MESA is huge so we'd rather avoid it.
         #:disallowed-references (,mesa)

         #:phases (modify-phases %standard-phases
                    ;; Use elogind instead of systemd.
                    (add-before 'configure 'remove-systemd
                      (lambda _
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
                           "1")))))))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("libxslt" ,libxslt)                       ;to build the man page
         ("libxml2" ,libxml2)                       ;for XML_CATALOG_FILES
         ("docbook-xsl" ,docbook-xsl)))
      (inputs
       `(("libdrm" ,libdrm)
         ("libtsm" ,libtsm)
         ("libxkbcommon" ,libxkbcommon)
         ("logind" ,elogind)
         ;; MESA can be used for accelerated video output via OpenGLESv2, but
         ;; it's a bit dependency that we'd rather avoid in the installation
         ;; image.
         ;; ("mesa" ,mesa)
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
                                 %supported-systems)))))

(define-public libtermkey
  (package
    (name "libtermkey")
    (version "0.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.leonerd.org.uk/code/libtermkey/"
                                  "libtermkey-" version ".tar.gz"))
              (sha256
               (base32 "002606rrxh5f6l6jrikl0dyxsknscdamq10av21xm0xa98ybsib9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list
                     (string-append "CC=" ,(cc-for-target))
                     (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'check 'patch-failing-test
           ;; XXX This undoes an upstream change in version 0.22 which ‘ensures
           ;; that the hooked function can invent TI strings for new terminal
           ;; types’.  That fails in the build environment.  Why?
           (lambda _
             (substitute* "t/40ti-override.c"
               (("vt750") "vt100")))))
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
    (version "3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/npat-efault/picocom")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vvjydqf0ax47nvdyyl67jafw5b3sfsav00xid6qpgia1gs2r72n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target)))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "picocom" bin)
               (install-file "picocom.1" man)))))))
    (home-page "https://github.com/npat-efault/picocom")
    (synopsis "Minimal dumb-terminal emulator")
    (description
     "Picocom is a minimal dumb-terminal emulation program.  It was designed to
serve as a simple and manual modem configuration, testing, and debugging tool.
It also serves well as a low-tech serial communications program to allow access
to all types of devices that provide serial consoles.")
    (license license:gpl2+)))

(define-public beep
  (package
    (name "beep")
    (version "1.4.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             ;; The original beep 1.3 at <http://www.johnath.com/beep> has been
             ;; unmaintained for some time, and vulnerable to at least two CVEs:
             ;; https://github.com/johnath/beep/issues/11#issuecomment-454056858
             ;; Use this maintained fork instead.
             (url "https://github.com/spkr-beep/beep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jmvqk6g5n0wzj9znw42njxq3mzw1769f4db99b83927hf4aidi4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out"))
             (string-append "pkgdocdir=$(docdir)/" ,name "-" ,version))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (synopsis "Linux command-line utility to control the PC speaker")
    (description "beep allows the user to control the PC speaker with precision,
allowing different sounds to indicate different events.  While it can be run
quite happily on the command line, its intended place of residence is within
scripts, notifying the user when something interesting occurs.  Of course, it
has no notion of what's interesting, but it's very good at that notifying part.")
    (home-page "https://github.com/spkr-beep/beep")
    (license license:gpl2+)))

(define-public unibilium
  (package
    (name "unibilium")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mauke/unibilium")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wa9a32wzqnxqh1jh554afj13dzjr6mw2wzqzw8d08nza9pg2ra2"))))
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
     `(("libtool" ,libtool)
       ("perl" ,perl)))
    (home-page "https://github.com/mauke/unibilium")
    (synopsis "Terminfo parsing library")
    (description "Unibilium is a basic C terminfo library.  It doesn't depend
on curses or any other library.  It also doesn't use global variables, so it
should be thread-safe.")
    (license license:lgpl3+)))

(define-public libvterm
  (package
    (name "libvterm")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.leonerd.org.uk/code/libvterm/"
                           "libvterm-" version ".tar.gz"))
       (sha256
        (base32
         "1pfkhbbihd2kvaza707vl2nvk7bxaawmb37wf9v6d72mjng38w5w"))))
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
  (let ((commit "1.1.1")
        (revision "0"))                 ;not used currently
    (package
      (name "cool-retro-term")
      (version "1.1.1")
      (source (origin
                (method git-fetch)
                (file-name (string-append name "-" version "-checkout"))
                (uri (git-reference
                      (url (string-append "https://github.com/Swordfish90/" name))
                      (commit commit)
                      (recursive? #t)))
                (sha256
                 (base32 "0wb6anchxa5jpn9c73kr4byrf2xlj8x8qzc5x7ny6saj7kbbvp75"))
                (modules '((guix build utils)
                           (srfi srfi-1)
                           (srfi srfi-26)
                           (ice-9 rdelim)
                           (ice-9 regex)))
                (snippet
                 '(let* ((fonts '(;"1971-ibm-3278"     ; BSD 3-clause
                                  "1977-apple2"        ; Non-Free
                                  "1977-commodore-pet" ; Non-Free
                                  "1979-atari-400-800" ; Non-Free
                                  ;"1981-ibm-pc        ; CC-SA 4.0
                                  "1982-commodore64")) ; Non-Free
                                  ;"1985-ibm-pc-vga"   ; CC-SA 4.0
                                  ;"modern-fixedsys-excelsior" ; Redistributable
                                  ;"modern-hermit"     ; SIL
                                  ;"modern-inconsolata"; SIL
                                  ;"modern-pro-font-win-tweaked" ; X11
                                  ;"modern-proggy-tiny"; X11
                                  ;"modern-terminus"   ; SIL
                         (name-rx (make-regexp " *name: *\"([^\"]*)\""))
                         (source-rx (make-regexp " *source: \"fonts/([^/]*)[^\"]*\""))
                         (fontname-rx (make-regexp "\"fontName\":\"([^\"]*).*"))
                         (names
                          ;; Gather font names from all Fonts*.qml files.
                          ;; These will be used to remove items from the
                          ;; default profiles.
                          (fold
                           (lambda (font-file names)
                             (call-with-input-file font-file
                               (lambda (port)
                                 (let loop ((name #f) (names names))
                                   (let ((line (read-line port)))
                                     (cond
                                      ((eof-object? line) (pk 'names names))
                                      ((regexp-exec name-rx line)
                                       => (lambda (m)
                                            (loop (match:substring m 1) names)))
                                      ((regexp-exec source-rx line)
                                       => (lambda (m)
                                            (let ((font (match:substring m 1)))
                                              (if (member font fonts)
                                                  (loop #f (lset-adjoin string=?
                                                                        names name))
                                                  (loop #f names)))))
                                      (else (loop name names))))))))
                           '() (find-files "app/qml" "Font.*\\.qml"))))
                    ;; Remove the font files themselves
                    (for-each (lambda (font)
                                (delete-file-recursively
                                 (string-append "app/qml/fonts/" font)))
                              fonts)
                    ;; Remove mention of those fonts in the source
                    (substitute* "app/qml/resources.qrc"
                      (((string-append " *<file>fonts/("
                                       (string-join fonts "|")
                                       ").*"))
                       ""))
                    (for-each
                     (lambda (file)
                       (let ((start-rx (make-regexp " *ListElement\\{"))
                             (end-rx   (make-regexp " *\\}")))
                        (with-atomic-file-replacement file
                          (lambda (in out)
                            (let loop ((line-buffer '())
                                       (hold? #f)
                                       (discard? #f))
                              (let ((line (read-line in 'concat)))
                                (cond
                                 ((eof-object? line) #t) ;done
                                 ((regexp-exec start-rx line)
                                  (loop (cons line line-buffer) #t #f))
                                 ((or (regexp-exec source-rx line)
                                      (regexp-exec fontname-rx line))
                                  => (lambda (m)
                                       (let ((font-or-name (match:substring m 1)))
                                         (if (or (member font-or-name fonts)
                                                 (member font-or-name names))
                                             (loop '() #f #t)
                                             (loop (cons line line-buffer)
                                                   hold? #f)))))
                                 ((regexp-exec end-rx line)
                                  (unless discard?
                                          (for-each (cut display <> out)
                                                    (reverse line-buffer))
                                          (display line out))
                                  (loop '() #f #f))
                                 (hold? (loop (cons line line-buffer)
                                              hold? discard?))
                                 (discard? (loop line-buffer #f #t))
                                 (else (display line out)
                                       (loop '() #f #f)))))))))
                     '("app/qml/FontPixels.qml"
                       "app/qml/FontScanlines.qml"
                       "app/qml/Fonts.qml"
                       "app/qml/ApplicationSettings.qml"))
                    ;; Final substitution for default scanline and pixel fonts
                    (substitute* "app/qml/ApplicationSettings.qml"
                      (("COMMODORE_PET") "PROGGY_TINY"))
                    #t))))
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
               (let ((out (assoc-ref outputs "out")))
                 (substitute* '("qmltermwidget/qmltermwidget.pro")
                   (("INSTALL_DIR = \\$\\$\\[QT_INSTALL_QML\\]")
                    (string-append "INSTALL_DIR = " out "/lib/qt5/qml")))
                 (substitute* '("cool-retro-term.pro" "app/app.pro")
                   (("/usr") out))
                 (invoke "qmake"))))
           (add-after 'install 'wrap-executable
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (qml "/lib/qt5/qml"))
                 (wrap-program (string-append out "/bin/cool-retro-term")
                   `("QML2_IMPORT_PATH" ":" prefix
                     (,(string-append out qml)
                      ,@(map (lambda (i)
                               (string-append (assoc-ref inputs i) qml))
                             '("qtdeclarative"
                               "qtgraphicaleffects"
                               "qtquickcontrols")))))
                 #t)))
           (add-after 'install 'add-alternate-name
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (symlink (string-append bin "/cool-retro-term")
                          (string-append bin "/crt"))
                 #t)))
           (add-after 'install 'install-man
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((mandir (string-append (assoc-ref outputs "out")
                                            "/share/man/man1")))
                 (install-file "packaging/debian/cool-retro-term.1" mandir)
                 #t))))))
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

(define-public foot
  (package
    (name "foot")
    (version "1.6.3")
    (home-page "https://codeberg.org/dnkl/foot")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rm7w29wf3gipf69qf7s42qw8857z74gsigrpz9g6vvd1x58f03m"))))
    (build-system meson-build-system)
    (arguments
     `(#:meson ,meson-0.55
       ;; Using a "release" build is recommended both for performance, and
       ;; also to address a GCC 10 issue when doing PGO builds.
       #:build-type "release"
       ;; Enable LTO as recommended by INSTALL.md.
       #:configure-flags '("-Db_lto=true")))
    (native-inputs
     `(("ncurses" ,ncurses)             ;for 'tic'
       ("pkg-config" ,pkg-config)
       ("scdoc" ,scdoc)
       ("wayland-protocols" ,wayland-protocols)))
    (inputs
     `(("fcft" ,fcft)
       ("libxkbcommon" ,libxkbcommon)
       ("wayland" ,wayland)))
    (synopsis "Wayland-native terminal emulator")
    (description
     "@command{foot} is a terminal emulator for systems using the Wayland
display server.  It is designed to be fast, lightweight, and independent of
desktop environments.  It can be used as a standalone terminal and also has
a server/client mode.")
    (license license:expat)))

(define-public sakura
  (package
    (name "sakura")
    (version "3.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/sakura/trunk/"
                                  version "/+download/sakura-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "12wjmckf03qbnm8cb7qma0980anzajn3l92rj2yr8hhafl74x6kj"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no check phase
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("perl" ,perl)                   ; for pod2man
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
  (let ((commit "288510b9734e30e7966ec2f22b87c5f8e67345e3")
        (revision "1"))
    (package
      (name "go-github.com-nsf-termbox-go")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nsf/termbox-go")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0hdyisfaf8yb55h3p03p4sbq19546mp9fy28f2kn659mycmhxqk4"))))
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

(define-public go-github-com-junegunn-fzf
  (package
    (name "go-github-com-junegunn-fzf")
    (version "0.25.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/junegunn/fzf")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1j5bfxl4w8w3n89p051y8dhxg0py9l98v7r2gkr63bg4lj32faz8"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/junegunn/fzf"))
    (inputs
     `(("go-github.com-mattn-go-runewidth" ,go-github.com-mattn-go-runewidth)
       ("go-github-com-mattn-go-shellwords" ,go-github-com-mattn-go-shellwords)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github-com-gdamore-tcell" ,go-github-com-gdamore-tcell)
       ("go-github-com-saracen-walker" ,go-github-com-saracen-walker)
       ("go-golang.org-x-sync-errgroup" ,go-golang.org-x-sync-errgroup)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (home-page "https://github.com/junegunn/fzf")
    (synopsis "Command-line fuzzy-finder")
    (description "This package provides an interactive command-line filter
usable with any list--including files, command history, processes and more.")
    (license license:expat)))

(define-public fzf
  (package
    (inherit go-github-com-junegunn-fzf)
    (name "fzf")
    (arguments
     (ensure-keyword-arguments
      (package-arguments go-github-com-junegunn-fzf)
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'copy-binaries
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (with-directory-excursion "src/github.com/junegunn/fzf"
                  (install-file "bin/fzf-tmux"
                                (string-append out "/bin"))))))
          (add-after 'copy-binaries 'wrap-programs
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out"))
                    (ncurses (assoc-ref inputs "ncurses")))
                (wrap-program (string-append out "/bin/fzf-tmux")
                  `("PATH" ":" prefix (,(string-append ncurses "/bin")))))))
          (add-after 'install 'install-completions
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bash-completion (string-append out "/etc/bash_completion.d"))
                     (zsh-completion (string-append out "/share/zsh/site-functions")))
                (with-directory-excursion "src/github.com/junegunn/fzf"
                  (mkdir-p bash-completion)
                  (copy-file "shell/completion.bash"
                             (string-append bash-completion "/fzf"))
                  (mkdir-p zsh-completion)
                  (copy-file "shell/completion.zsh"
                             (string-append zsh-completion "/_fzf"))))))))))
    (inputs
     `(,@(package-inputs go-github-com-junegunn-fzf)
       ("ncurses" ,ncurses)))))

(define-public go-github.com-howeyc-gopass
  (let ((commit "bf9dde6d0d2c004a008c27aaee91170c786f6db8")
        (revision "0"))
    (package
      (name "go-github.com-howeyc-gopass")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/howeyc/gopass")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jxzyfnqi0h1fzlsvlkn10bncic803bfhslyijcxk55mgh297g45"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/howeyc/gopass"))
      (propagated-inputs
       `(("go-golang-org-x-crypto"
          ,go-golang-org-x-crypto)))
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
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "blessings" version))
       (sha256
        (base32
         "0z8mgkbmisxs10rz88qg46l1c9a8n08k8cy2iassal2zh16qbrcq"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Test suite is unable to detect TTY conditions.
     `(#:tests? #f))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("python-six" ,python-six)))
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
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "curtsies" version))
       (sha256
        (base32
         "019bpf5wmng4f6ic2ykg893ypfihpfvzi6dhblcagfwbincl79ac"))))
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
    (home-page "https://github.com/bpython/curtsies")
    (synopsis "Library for curses-like terminal interaction with colored
strings")
    (description "Curtsies is a Python library for interacting with the
terminal.  It features string-like objects which carry formatting information,
per-line fullscreen terminal rendering, and keyboard input event reporting.")
    (license license:expat)))

(define-public tmate
  (package
    (name "tmate")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tmate-io/tmate")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0x5c31yq7ansmiy20a0qf59wagba9v3pq97mlkxrqxn4n1gcc6vi"))))
    (build-system gnu-build-system)
    (inputs
     `(("libevent" ,libevent)
       ("libssh" ,libssh)
       ("msgpack" ,msgpack)
       ("ncurses" ,ncurses)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (home-page "https://tmate.io/")
    (synopsis "Terminal sharing application")
    (description "tmate is a terminal sharing application that allows you to
share your terminal with other users over the Internet.  tmate is a fork of
tmux.")
    (license license:isc)))

(define-public kitty
  (package
    (name "kitty")
    (version "0.19.3")
    (home-page "https://sw.kovidgoyal.net/kitty/")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kovidgoyal/kitty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r49bybqy6c0n1lz6yc85py80wb40w757m60f5rszjf200wnyl6s"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; patch needed as sphinx-build is used as a python script
           ;; whereas the guix package uses a bash script launching the
           ;; python script
           (substitute* "docs/conf.py"
             (("(from kitty.constants import str_version)" kitty-imp)
              (string-append "sys.path.append(\"..\")\n" kitty-imp)))
           (substitute* "docs/Makefile"
             (("^SPHINXBUILD[[:space:]]+= (python3.*)$")
              "SPHINXBUILD = sphinx-build\n"))
           #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("libdbus" ,dbus)
       ("libgl1-mesa" ,mesa)
       ("libxcursor" ,libxcursor)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxkbcommon" ,libxkbcommon)
       ("libxrandr" ,libxrandr)
       ("ncurses" ,ncurses) ;; for tic command
       ("pkg-config" ,pkg-config)
       ("sphinx" ,python-sphinx)
       ("wayland-protocols" ,wayland-protocols)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("harfbuzz" ,harfbuzz)
       ("lcms" ,lcms)
       ("libcanberra" ,libcanberra)
       ("libpng" ,libpng)
       ("pygments" ,python-pygments)
       ("python" ,python-wrapper)
       ("wayland" ,wayland)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)   ;no configure script
                  (replace 'build
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; The "kitty" sub-directory must be writable prior to
                      ;; configuration (e.g., un-setting updates).
                      (for-each make-file-writable (find-files "kitty"))

                      (invoke "python3" "setup.py" "linux-package"
                              ;; Do not phone home.
                              "--update-check-interval=0"
                              ;; Wayland backend requires EGL, which isn't
                              ;; found out-of-the-box for some reason.
                              (string-append "--egl-library="
                                             (assoc-ref inputs "libgl1-mesa")
                                             "/lib/libEGL.so.1"))))
                  (replace 'check
                    (lambda _
                      ;; Fix "cannot find kitty executable" error when running
                      ;; tests.
                      (setenv "PATH" (string-append "linux-package/bin:"
                                                    (getenv "PATH")))
                      (invoke "python3" "test.py")))
                  (add-before 'install 'rm-pycache
                    ;; created python cache __pycache__ are non deterministic
                    (lambda _
                      (let ((pycaches (find-files "linux-package/"
                                                  "__pycache__"
                                                  #:directories? #t)))
                        (for-each delete-file-recursively pycaches)
                        #t)))
                  (replace 'install
                    (lambda _
                      (let* ((out (assoc-ref %outputs "out"))
                             (obin (string-append out "/bin"))
                             (olib (string-append out "/lib"))
                             (oshare (string-append out "/share")))
                        (copy-recursively "linux-package/bin" obin)
                        (copy-recursively "linux-package/share" oshare)
                        (copy-recursively "linux-package/lib" olib)
                        #t))))))
    (synopsis "Fast, featureful, GPU based terminal emulator")
    (description "Kitty is a fast and featureful GPU-based terminal emulator:
@itemize
@item Offloads rendering to the GPU for lower system load and buttery smooth
scrolling.  Uses threaded rendering to minimize input latency.
@item Supports all modern terminal features: graphics (images), unicode,
true-color, OpenType ligatures, mouse protocol, focus tracking, bracketed
paste and several new terminal protocol extensions.
@item Supports tiling multiple terminal windows side by side in different
layouts without needing to use an extra program like tmux.
@item Can be controlled from scripts or the shell prompt, even over SSH.
@item Has a framework for Kittens, small terminal programs that can be used to
extend kitty's functionality.  For example, they are used for Unicode input,
hints, and side-by-side diff.
@item Supports startup sessions which allow you to specify the window/tab
layout, working directories and programs to run on startup.
@item Allows you to open the scrollback buffer in a separate window using
arbitrary programs of your choice.  This is useful for browsing the history
comfortably in a pager or editor.
@end itemize")
    (license license:gpl3+)))

(define-public eternalterminal
  (package
    (name "eternalterminal")
    (version "6.0.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/MisterTea/EternalTerminal")
               (commit (string-append "et-v" version))))
        (file-name (git-file-name name version))
       (sha256
        (base32 "0sb1hypg2276y8c2a5vivrkcxp70swddvhnd9h273if3kv6j879r"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_TEST=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'insert-googletests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((tests (assoc-ref inputs "googletest")))
               (copy-recursively tests "external/googletest"))
             #t)))))
    (inputs
     `(("gflags" ,gflags)
       ("libsodium" ,libsodium)
       ("protobuf" ,protobuf)))
    (native-inputs
     `(("googletest" ,(package-source googletest))))
    (home-page "https://mistertea.github.io/EternalTerminal/")
    (synopsis "Remote shell that reconnects without interrupting the session")
    (description "@dfn{Eternal Terminal} (ET) is a remote shell that
automatically reconnects without interrupting the session.  ET uses SSH to
initialize a secure connection.  Unlike SSH sessions, which must be killed and
reconnected after a network outage an ET session will survive network outages
and IP roaming.  ET provides the same core functionality as @command{mosh},
while also supporting native scrolling and @command{tmux} control mode
(@code{tmux -CC}).")
    (license license:asl2.0)))

(define-public et
  (deprecated-package "et" eternalterminal))

(define-public wterm
  (package
    (name "wterm")
    (version "0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/majestrate/wterm")
             (commit "0ae42717c08a85a6509214e881422c7fbe7ecc45")))
       (sha256
         (base32
          "0g4lzmc1w6na81i6hny32xds4xfig4xzswzfijyi6p93a1226dv0"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libdrm" ,libdrm)
       ("libxkbcommon" ,libxkbcommon)
       ("ncurses" ,ncurses)
       ("pixman" ,pixman)
       ("wayland" ,wayland)))
    (arguments
     '(#:tests? #f

       ;; Without -j1 it fails to find file libwld.a.
       #:parallel-build? #f

       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output)
                          (string-append "TERMINFO="
                                         (assoc-ref %outputs "out")
                                         "/share/terminfo"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'terminfo-fix
           (lambda _
             (substitute* "Makefile"
               (("\ttic .*") "\tmkdir -p $(SHARE_PREFIX)/share/terminfo
\ttic -o $(SHARE_PREFIX)/share/terminfo -s wterm.info\n"))
             #t)))))
    (native-search-paths
      (list (search-path-specification
              (variable "TERMINFO_DIRS")
              (files '("share/terminfo")))))
    (home-page "https://github.com/majestrate/wterm")
    (synopsis "Terminal emulator for Wayland")
    (description "wterm is a native Wayland terminal emulator based on
an st fork using wld. st is a simple terminal emulator for X originally
made by suckless.")
    (license license:x11)))

(define-public alacritty
  (package
    (name "alacritty")
    (version "0.7.1")
    (source
     (origin
       ;; XXX: The crate at "crates.io" has limited contents.  In particular,
       ;; it does not contain "extra" directory with completions, icon, etc.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jwilm/alacritty")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b9hy3ya72hhpl8nkayc7dy4f97xp75np48dm5na5pgyv8b45agi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f     ; virtual manifest
       #:cargo-test-flags '("--release" "--" "--skip=config_read_eof")
       #:cargo-inputs
       (("rust-alacritty-config-derive" ,rust-alacritty-config-derive-0.1)
        ("rust-alacritty-terminal" ,rust-alacritty-terminal-0.12)
        ("rust-clap" ,rust-clap-2)
        ("rust-cocoa" ,rust-cocoa-0.24)
        ("rust-copypasta" ,rust-copypasta-0.7)
        ("rust-crossfont" ,rust-crossfont-0.2)
        ("rust-embed-resource" ,rust-embed-resource-1)
        ("rust-fnv" ,rust-fnv-1)
        ("rust-gl-generator" ,rust-gl-generator-0.14)
        ;; XXX: Adjust `add-absolute-library-references' phase when updating
        ;; glutin input.
        ("rust-glutin" ,rust-glutin-0.26)
        ("rust-log" ,rust-log-0.4)
        ("rust-notify" ,rust-notify-4)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-parking-lot" ,rust-parking-lot-0.11)
        ("rust-png" ,rust-png-0.16)
        ("rust-raw-window-handle" ,rust-raw-window-handle-0.3)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-time" ,rust-time-0.1)
        ("rust-urlocator" ,rust-urlocator-0.1)
        ("rust-x11-dl" ,rust-x11-dl-2)
        ("rust-xdg" ,rust-xdg-2))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'add-absolute-library-references
           (lambda* (#:key inputs cargo-inputs vendor-dir #:allow-other-keys)
             (let* ((glutin-name ,(package-name rust-glutin-0.26))
                    (glutin-version ,(package-version rust-glutin-0.26))
                    (glutin-api (string-append glutin-name "-" glutin-version
                                               ".tar.gz/src/api/"))
                    (smithay-client-toolkit-name
                     ,(package-name rust-smithay-client-toolkit-0.12))
                    (smithay-client-toolkit-version
                     ,(package-version rust-smithay-client-toolkit-0.12))
                    (smithay-client-toolkit-src
                     (string-append smithay-client-toolkit-name "-"
                                    smithay-client-toolkit-version ".tar.gz/src"))
                    (libxkbcommon (assoc-ref inputs "libxkbcommon"))
                    (mesa (assoc-ref inputs "mesa")))
               ;; Fix dlopen()ing some libraries on pure Wayland (no $DISPLAY):
               ;; Failed to initialize any backend! Wayland status: NoWaylandLib
               ;; XXX We patch transitive dependencies that aren't even direct
               ;; inputs to this package, because of the way Guix's Rust build
               ;; system currently works.  <http://issues.guix.gnu.org/46399>
               ;; might fix this and allow patching them directly.
               (substitute* (string-append vendor-dir "/"
                                           smithay-client-toolkit-src
                                           "/seat/keyboard/ffi.rs")
                 (("libxkbcommon\\.so")
                  (string-append libxkbcommon "/lib/libxkbcommon.so")))

               ;; Mesa is needed everywhere.
               (substitute*
                   (string-append vendor-dir "/" glutin-api "glx/mod.rs")
                 (("libGL.so") (string-append mesa "/lib/libGL.so")))
               (substitute*
                   (string-append vendor-dir "/" glutin-api "egl/mod.rs")
                 (("libEGL.so") (string-append mesa "/lib/libEGL.so")))
               #t)))
         (replace 'install
           ;; Upstream install script only takes care of executable.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (share (string-append out "/share"))
                    (icons (string-append share "/icons/hicolor/scalable/apps"))
                    (tic   (string-append (assoc-ref inputs "ncurses") "/bin/tic"))
                    (man   (string-append share "/man/man1"))
                    (alacritty-bin "target/release/alacritty"))
               ;; Install the executable.
               (install-file alacritty-bin bin)
               ;; Install man pages.
               (mkdir-p man)
               (copy-file "extra/alacritty.man"
                          (string-append man "/alacritty.1"))
               ;; Install desktop file.
               (install-file "extra/linux/Alacritty.desktop"
                             (string-append share "/applications"))
               ;; Install icon.
               (mkdir-p icons)
               (copy-file "extra/logo/alacritty-term.svg"
                          (string-append icons "/Alacritty.svg"))
               ;; Install terminfo.
               (mkdir-p (string-append share "/terminfo"))
               ;; We don't compile alacritty-common entry because
               ;; it's being used only for inheritance.
               (invoke tic "-x" "-e" "alacritty,alacritty-direct"
                       "-o" (string-append share "/terminfo/")
                       "extra/alacritty.info")
               ;; Install completions.
               (install-file "extra/completions/alacritty.bash"
                             (string-append out "/etc/bash_completion.d"))
               (install-file "extra/completions/_alacritty"
                             (string-append share "/zsh/site-functions"))
               (install-file "extra/completions/alacritty.fish"
                             (string-append share "/fish/vendor_completions.d"))
               #t))))))
    (native-inputs
     `(("cmake" ,cmake)
       ("ncurses" ,ncurses)
       ("pkg-config" ,pkg-config)
       ("python3" ,python)))
    (inputs
     `(("expat" ,expat)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxcursor" ,libxcursor)
       ("libxi" ,libxi)
       ("libxkbcommon" ,libxkbcommon)
       ("libxrandr" ,libxrandr)
       ("libxxf86vm" ,libxxf86vm)
       ("mesa" ,mesa)
       ("rust-bitflags" ,rust-bitflags-1)
       ("rust-dirs" ,rust-dirs-2)
       ("rust-libc" ,rust-libc-0.2)
       ("rust-unicode-width" ,rust-unicode-width-0.1)
       ("rust-wayland-client" ,rust-wayland-client-0.28)
       ("rust-winapi" ,rust-winapi-0.3)
       ("wayland" ,wayland)))
    (native-search-paths
     ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
     ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
     (list (search-path-specification
            (variable "TERMINFO_DIRS")
            (files '("share/terminfo")))))
    (home-page "https://github.com/alacritty/alacritty")
    (synopsis "GPU-accelerated terminal emulator")
    (description
     "Alacritty is a GPU-accelerated terminal emulator with a strong focus on
simplicity and performance.  With such a strong focus on performance, included
features are carefully considered and you can always expect Alacritty to be
blazingly fast.  By making sane choices for defaults, Alacritty requires no
additional setup.  However, it does allow configuration of many aspects of the
terminal.  Note that you need support for OpenGL 3.2 or higher.")
    (license license:asl2.0)))

(define-public bootterm
  (package
    (name "bootterm")
    (version "0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/wtarreau/bootterm")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k3jacld98za41dbpr10sjms77hrw91sb10m0cnwv3h7aifiwmrs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no test suite
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; No ./configure script
         (delete 'configure)
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "README.md" doc)
               #t))))))
    (home-page "https://github.com/wtarreau/bootterm")
    (synopsis "Serial terminal")
    (description "Bootterm is a terminal designed to ease connection to
ephemeral serial ports.  It features automatic port detection, port enumeration,
support for non-standard baud rates, the ability to wait for ports to appear,
and the ability to read and write via stdin and stdout.")
    (license license:expat)))
