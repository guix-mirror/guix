;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Brendan Tildesley <brendan.tildesley@openmailbox.org>
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
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages text-editors)
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
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/pcmanfm/"
                                  "PCManFM%20%2B%20Libfm%20%28tarball%20release"
                                  "%29/LibFM/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0nlvfwh09gbq8bkbvwnw6iqr918rrs9gc9ljb9pjspyg408bn1n7"))))
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
    (home-page "http://lxde.org")
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
    (home-page "http://lxde.org")
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
    (home-page "http://lxde.org")
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
    (home-page "http://lxde.org")
    (license license:gpl2+)))

(define-public lxterminal
  (package
    (name "lxterminal")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lxde/LXTerminal"
                                  "%20%28terminal%20emulator%29/LXTerminal%20"
                                  version "/" name "-" version ".tar.xz"))
              (patches (search-patches "lxterminal-CVE-2016-10369.patch"))
              (sha256
               (base32
                "1yf76s15zvfw0h42b0ay1slpq47khgjmcry8ki2z812zar9lchia"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Tests for "po" fail with "No rule to make target '../src/encoding.c'
       ;; needed by 'lxterminal.pot'. Stop."
       #:tests? #f))
    (inputs `(("gtk+" ,gtk+-2)
              ("vte"  ,vte/gtk+-2)))
    (native-inputs `(("intltool"   ,intltool)
                     ("pkg-config" ,pkg-config)))
    (synopsis "LXDE terminal emulator")
    (description "LXTerminal is a VTE-based terminal emulator.  It supports
multiple tabs and has only minimal dependencies thus being completely
desktop-independent.  In order to reduce memory usage and increase the
performance, all instances of the terminal are sharing a single process.")
    (home-page "http://lxde.org")
    (license license:gpl2+)))

(define-public menu-cache
  (package
    (name "menu-cache")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/lxde/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1m8j40npykfcfqs43kc0fmksal2jfmfi8lnb3mq3xy1lvvrfv0vg"))))
    (build-system gnu-build-system)
    (inputs `(("glib"  ,glib)
              ("libfm" ,libfm-extra)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (synopsis "LXDE implementation of the freedesktop menu's cache")
    (description "Menu-cache is a library creating and utilizing caches to
speed up the access to freedesktop.org defined application menus.")
    (home-page "http://lxde.org")
    (license license:lgpl2.1+)))

(define-public pcmanfm
  (package
    (name "pcmanfm")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/"
                                  "PCManFM%20%2B%20Libfm%20%28tarball%20release"
                                  "%29/PCManFM/" name "-" version ".tar.xz"))
              (patches (search-patches "pcmanfm-CVE-2017-8934.patch"))
              (sha256
               (base32
                "0rxdh0dfzc84l85c54blq42gczygq8adhr3l9hqzy1dp530cm1hc"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+"   ,gtk+-2)
              ("gvfs"   ,gvfs)                    ;for trash and mount support
              ("libfm"  ,libfm)
              ("libx11" ,libx11)))
    (native-inputs `(("intltool"   ,intltool)
                     ("libtool"    ,libtool)
                     ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("lxmenu-data" ,lxmenu-data)))     ;for "Open With..." application list
    (synopsis "LXDE file manager")
    (description "PCMan is a lightweight GTK+ based file manager, compliant
with freedesktop.org standard.")
    (home-page "http://lxde.org")
    (license license:gpl2+)))

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
    (home-page "http://lxde.org")
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
    (home-page "http://lxde.org")
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
    (home-page "http://lxde.org")
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
    (home-page "http://lxde.org")
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
                     (find-files "." generated-c-file?))))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'rm-stamp
           (lambda _
             (for-each delete-file (find-files "." "\\.stamp$"))))
         (add-after 'rm-stamp 'autoreconf
           (lambda _
             (zero? (system* "autoreconf" "-vfi")))))))
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
    (home-page "http://lxde.org")
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
    (home-page "http://lxde.org")
    (license license:gpl2+)))

(define-public lxde
  (package
    (name "lxde")
    (version (package-version lxde-common))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (mkdir %output)))
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
