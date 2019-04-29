;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Brendan Tildesley <brendan.tildesley@openmailbox.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 ison <ison@airmail.cc>
;;; Copyright © 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Meiyo Peng <meiyo@riseup.net>
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

(define-module (gnu packages lxde)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public libfm
  (package
    (name "libfm")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/pcmanfm/"
                                  "PCManFM%20%2B%20Libfm%20%28tarball%20release"
                                  "%29/LibFM/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1r6gl49xrykldwz8y4h2s7gjarxigg3bbkrj0gphxjj1vr5j9ccn"))))
    (build-system gnu-build-system)
    (inputs `(("glib" ,glib)
              ("gtk+" ,gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("glib"       ,glib "bin") ; for gtester
                     ("libtool"    ,libtool)
                     ("menu-cache" ,menu-cache)
                     ("pkg-config" ,pkg-config)
                     ("vala"       ,vala)))
    (synopsis "File management support (core library)")
    (description "LibFM provides file management functions built on top of
Glib/GIO giving a higher-level API.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))

(define-public libfm-extra
  (package (inherit libfm)
    (name "libfm-extra")
    (arguments '(#:configure-flags '("--with-extra-only")))
    (inputs `(("glib" ,glib)))
    (native-inputs `(("intltool"   ,intltool)
                     ("libtool"    ,libtool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "File management support (extra library)")
    (description "This package contains standalone library which extends the
libFM file management library.")))

(define-public lxappearance
  (package
    (name "lxappearance")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lxde/"
                                  "LXAppearance/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "07r0xbi6504zjnbpan7zrn7gi4j0kbsqqfpj8v2x94gr05p16qj4"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "LXDE GTK+ theme switcher")
    (description "LXAppearance is a desktop-independent GTK+ theme switcher
able to change themes, icons, and fonts used by GTK+ applications.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))

(define-public lxrandr
  (package
    (name "lxrandr")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lxde/LXRandR"
                                  "%20%28monitor%20config%20tool%29/LXRandR%20"
                                  (version-major+minor version) ".x/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0khqi42paqg82jllb2kza4arf3fafzgq90fhyr3rw3d9hn23763d"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "LXDE monitor configuration tool")
    (description "LXRandR is a very basic monitor configuration tool.  It
relies on the X11 resize-and-rotate (RandR) extension but doesn't aim to be a
full frontend of it.  LXRandR only gives you some easy and quick options which
are intuitive.  It's suitable for laptop users who frequently uses projectors
or external monitor.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))

(define-public lxtask
  (package
    (name "lxtask")
    (version "0.1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lxde/LXTask"
                                  "%20%28task%20manager%29/LXTask%20"
                                  (version-major+minor version) ".x/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1zihhvzsg9bl6k0gv7jwx6cgsi3rmcagvnmshc1h0mjq2immmdph"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "LXDE task manager")
    (description "LXTask is a lightweight task manager derived from Xfce task
manager with all dependencies on Xfce removed.  LXTask is based on the GTK+
toolkit.  It allows users to monitor and control of running processes.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))

(define-public lxterminal
  (package
    (name "lxterminal")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lxde/LXTerminal"
                                  "%20%28terminal%20emulator%29/LXTerminal%20"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1124pghrhnx6q4391ri8nvi6bsmvbj1dx81an08mird8jf2b2rii"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+-2)
              ("vte"  ,vte/gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "LXDE terminal emulator")
    (description "LXTerminal is a VTE-based terminal emulator.  It supports
multiple tabs and has only minimal dependencies thus being completely
desktop-independent.  In order to reduce memory usage and increase the
performance, all instances of the terminal are sharing a single process.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))

(define-public menu-cache
  (package
    (name "menu-cache")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lxde/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1iry4zlpppww8qai2cw4zid4081hh7fz8nzsp5lqyffbkm2yn0pd"))))
    (build-system gnu-build-system)
    (inputs `(("glib"  ,glib)
              ("libfm" ,libfm-extra)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (synopsis "LXDE implementation of the freedesktop menu's cache")
    (description "Menu-cache is a library creating and utilizing caches to
speed up the access to freedesktop.org defined application menus.")
    (home-page "https://lxde.org")
    (license license:lgpl2.1+)))

(define-public pcmanfm
  (package
    (name "pcmanfm")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/pcmanfm/"
                                  "PCManFM%20%2B%20Libfm%20%28tarball%20release"
                                  "%29/PCManFM/pcmanfm-" version ".tar.xz"))
              (sha256
               (base32
                "0mb8hg76x1z0szdyl0w7jpz0bdblc6a29is1vvnh79z37qxh8138"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+"   ,gtk+-2)
              ("gvfs"   ,gvfs)          ; for trash and mount support
              ("libfm"  ,libfm)
              ("libx11" ,libx11)))
    (native-inputs `(("intltool"   ,intltool)
                     ("libtool"    ,libtool)
                     ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("lxmenu-data" ,lxmenu-data)))   ; for "Open With..." application list
    (synopsis "LXDE file manager")
    (description "PCMan is a lightweight GTK+ based file manager, compliant
with freedesktop.org standard.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))

(define-public spacefm
  ;; SpaceFM is based on PCManFM.
  (package
    (name "spacefm")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/IgnorantGuru/spacefm/archive/"
                    version ".tar.gz"))
              (sha256
                (base32
                 "1jg7xfyr7kihjnalxp8wxyb9qjk8hqf5l36rp3s0lvkpmpyakppy"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "src/main.c"
                    (("#include <sys/types\\.h>" all)
                     ;; Add missing include for 'major' and 'minor' with glibc
                     ;; >= 2.28.
                     (string-append all "\n"
                                    "#include <sys/sysmacros.h>\n")))
                  #t))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("intltool" ,intltool)))
    (inputs `(("bash" ,bash)
              ("gtk+" ,gtk+)
              ("eudev" ,eudev)
              ("desktop-file-utils" ,desktop-file-utils)
              ("shared-mime-info" ,shared-mime-info)
              ("ffmpegthumbnailer" ,ffmpegthumbnailer)
              ("jmtpfs" ,jmtpfs)
              ("lsof" ,lsof)
              ("udisks" ,udisks)))
    (arguments
     `(#:configure-flags (list (string-append "--with-bash-path="
                                              (assoc-ref %build-inputs "bash")
                                              "/bin/bash")
                               (string-append "--sysconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc"))))
    (home-page "http://ignorantguru.github.io/spacefm/")
    (synopsis "Multi-panel tabbed file manager")
    (description "SpaceFM is a graphical, multi-panel, tabbed file manager
based on PCManFM with built-in virtual file system, udev-based device manager,
customizable menu system, and Bash integration.")

    ;; The combination is GPLv3+ but src/exo is under LGPLv3+.
    (license license:gpl3+)))

(define-public lxmenu-data
  (package
    (name "lxmenu-data")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.sourceforge.net/lxde/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1f5sh2dvb3pdnjlcsyzq9543ck2jsqizkx3204cr22zm5s6j3qwz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (synopsis "Freedesktop.org desktop menus for LXDE")
    (description
     "Lxmenu-data provides files required to build freedesktop.org
menu spec-compliant desktop menus for LXDE.")
    (home-page "https://lxde.org")
    (license license:lgpl2.1+)))

(define-public lxde-icon-theme
  (package
    (name "lxde-icon-theme")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.sourceforge.net/lxde/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0v4i6x86fr2hbx4fb2si7y2qzmj7h6hcjwaifnin18r8kwwvgl73"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "LXDE default icon theme based on nuoveXT2")
    (description
     "Lxde-icon-theme provides an default icon theme for LXDE.")
    (home-page "https://lxde.org")
    (license license:lgpl3)))

(define-public lxde-common
  (package
    (name "lxde-common")
    (version "0.99.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.sourceforge.net/lxde/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0mj84fa3f4ak1jjslrwc2q3ci9zxrxpciggviza9bjb0168brn8w"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-lxsession
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Set the right file name for 'lxsession'.
                      (let ((lxsession (assoc-ref inputs "lxsession")))
                        (substitute* "startlxde.in"
                          (("^exec .*/bin/lxsession")
                           (string-append "exec " lxsession
                                          "/bin/lxsession")))
                        #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("lxmenu-data" ,lxmenu-data)
       ("lxde-icon-theme" ,lxde-icon-theme)))
    (inputs
     `(("lxsession" ,lxsession)
       ;; ("lxlock" ,lxlock) ;for 'lxde-screenlock.desktop'
       ))
    (synopsis "Common files of the LXDE Desktop")
    (description
     "Lxde-common provides common files of the LXDE Desktop.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))

(define-public lxinput
  (package
    (name "lxinput")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.sourceforge.net/lxde/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "123f3yn4rp1w5b3n5aj3ad9snkxab29qkrs7bcvf5bx4cn57g3sf"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk+-2" ,gtk+-2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (synopsis "Tool for mouse and keyboard configuration in LXDE")
    (description
     "Lxinput provides a small program to configure keyboard and mouse
in LXDE.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))

(define-public lxsession
  (package
    (name "lxsession")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.sourceforge.net/lxde/"
                           name "-" version ".tar.xz"))
       (patches (search-patches "lxsession-use-gapplication.patch"))
       (sha256
        (base32
         "1a0zmyywwzdh59nc0l94cir18vhp633z4q2xfhn5zx11ajj45gwh"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove C files generated by Vala so we can build from source.
        '(let* ((c->vala
                 (lambda (file)
                   (string-append (string-drop-right file 2)
                                  ".vala")))
                (generated-c-file?
                 (lambda (file stat)
                   (and (string-suffix? ".c" file)
                        (file-exists? (c->vala file))))))
           (for-each delete-file
                     (find-files "." generated-c-file?))
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'rm-stamp
           (lambda _
             (for-each delete-file (find-files "." "\\.stamp$"))
             ;; Force regeneration of configure script.
             (delete-file "configure")
             #t)))))
    (inputs
     `(("gtk+-2" ,gtk+-2)
       ("polkit" ,polkit)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("docbook-xsl" ,docbook-xsl)
       ("vala" ,vala)
       ("autoconf" ,autoconf)
       ("automake" ,automake)))
    (synopsis "Lightweight X11 session manager")
    (description
     "Lxsession provides an lightweight X11 session manager.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))

(define-public lxpanel
  (package
    (name "lxpanel")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://downloads.sourceforge.net/lxde/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1ccgv7jgl3y865cpb6w7baaz7468fxncm83bqxlwyni5bwhglb1l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (menu (assoc-ref inputs "lxmenu-data")))
               (wrap-program (string-append out "/bin/lxpanel")
                 `("XDG_DATA_DIRS" ":" prefix
                   (,(string-append menu "/share"))))
               #t))))))
    (inputs
     ;; TODO: libindicator-0.3.0
     `(("gtk+-2" ,gtk+-2)
       ("alsa-lib" ,alsa-lib)
       ("libwnck-2" ,libwnck-2)
       ("keybinder" ,keybinder)
       ("libxmu" ,libxmu)
       ("libxpm" ,libxpm)
       ("libxml2" ,libxml2)
       ("cairo" ,cairo)
       ("libx11" ,libx11)
       ("wireless-tools" ,wireless-tools)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("docbook-xml" ,docbook-xml)
       ("gettext-minimal" ,gettext-minimal)))
    (propagated-inputs
     `(("lxmenu-data" ,lxmenu-data)
       ("libfm" ,libfm)
       ("menu-cache" ,menu-cache)))
    (synopsis "X11 Desktop panel for LXDE")
    (description
     "Lxpanel provides an X11 desktop panel for LXDE.")
    (home-page "https://lxde.org")
    (license license:gpl2+)))

(define-public lxde
  (package
    (name "lxde")
    (version (package-version lxde-common))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (begin (mkdir %output) #t)))
    (propagated-inputs
     ;; TODO:
     ;; lxshortcut, lxsession-edit
     ;; lxappearance-obconf
     `(("menu-cache" ,menu-cache)
       ("gpicview" ,gpicview)
       ("leafpad" ,leafpad)
       ("lxappearance" ,lxappearance)
       ("lxde-icon-theme" ,lxde-icon-theme)
       ("lxde-common" ,lxde-common)
       ("lxmenu-data" ,lxmenu-data)
       ("lxpanel" ,lxpanel)
       ("lxrandr" ,lxrandr)
       ("lxsession" ,lxsession)
       ("libfm" ,libfm)
       ("libfm-extra" ,libfm-extra)
       ("lxtask" ,lxtask)
       ("lxterminal" ,lxterminal)
       ("pcmanfm" ,pcmanfm)
       ("openbox" ,openbox)
       ("obconf" ,obconf)))
    (synopsis "Lightweight X11 Desktop Environment")
    (description
     "LXDE, which stands for Lightweight X11 Desktop Environment, is a
desktop environment which is lightweight and fast.  It is designed to be
user friendly and slim, while keeping the resource usage low.  LXDE uses
less RAM and less CPU while being a feature rich desktop environment.  Unlike
other tightly integrated desktops LXDE strives to be modular, so each
component can be used independently with few dependencies.")
    (home-page "https://lxde.org")
    (license license:gpl2+))) ; And others.

;;; lxde.scm ends here
