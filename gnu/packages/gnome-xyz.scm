;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Leo Prikler <leo.prikler@student.tugraz.at>
;;; Copyright © 2019, 2021 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2020 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Ellis Kenyo <me@elken.dev>
;;; Copyright © 2020 Stefan Reichör <stefan@xsteve.at>
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

(define-module (gnu packages gnome-xyz)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public arc-icon-theme
  (package
    (name "arc-icon-theme")
    (version "20161122")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/horst3180/arc-icon-theme")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ch3hp08qri93510hypzz6m2x4xgg2h15wvnhjwh1x1s1b7jvxjd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-configure-during-bootstrap
           (lambda _
             (substitute* "autogen.sh"
               (("^\"\\$srcdir/configure\".*") ""))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    ;; When Arc is missing an icon, it looks in the Moka icon theme for it.
    (propagated-inputs
     `(("moka-icon-theme" ,moka-icon-theme)))
    (synopsis "Arc icon theme")
    (description "The Arc icon theme provides a set of icons matching the
style of the Arc GTK theme.  Icons missing from the Arc theme are provided by
the Moka icon theme.")
    (home-page "https://github.com/horst3180/arc-icon-theme")
    (license license:gpl3+)))

(define-public delft-icon-theme
  (package
    (name "delft-icon-theme")
    (version "1.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/madmaxms/iconpack-delft")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1iw85cxx9lv7irs28qi3815dk9f9vldv2j7jf1x5l1dqzwaxgwpb"))
       (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("." "share/icons" #:exclude ("README.md" "LICENSE" "logo.jpg")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-index.theme
           (lambda _
            (substitute* "Delft/index.theme"
              (("gnome") "Adwaita"))
            #t)))))
    (home-page "https://www.gnome-look.org/p/1199881/")
    (synopsis "Continuation of Faenza icon theme with up to date app icons")
    (description "Delft is a fork of the popular icon theme Faenza with up to
date app icons.  It will stay optically close to the original Faenza icons,
which haven't been updated for some years.  The new app icons are ported from
the Obsidian icon theme.")
    (license license:gpl3)))

(define-public faba-icon-theme
  (package
    (name "faba-icon-theme")
    (version "4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snwh/faba-icon-theme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xh6ppr73p76z60ym49b4d0liwdc96w41cc5p07d48hxjsa6qd6n"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'disable-post-install
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script.*") "")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (synopsis "Faba icon theme")
    (description
     "Faba is a minimal icon set used as a basis for other themes such as
Moka")
    (home-page "https://snwh.org/moka")
    (license (list license:lgpl3+
                   license:cc-by-sa4.0))))

(define-public moka-icon-theme
  (package
    (inherit faba-icon-theme)
    (name "moka-icon-theme")
    (version "5.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/snwh/moka-icon-theme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "015l02im4mha5z91dbchxf6xkp66d346bg3xskwg0rh3lglhjsrd"))))
    (propagated-inputs
     ;; Moka is based on Faba by using it as a fallback icon set instead of
     ;; bundling it, so we need to add it as a propagated input.
     `(("faba-icon-theme" ,faba-icon-theme)))
    (synopsis "Moka icon theme")
    (description "Moka is a stylized desktop icon set, designed to be clear,
simple and consistent.")
    (license (list license:gpl3+
                   license:cc-by-sa4.0))))

(define-public papirus-icon-theme
  (package
    (name "papirus-icon-theme")
    (version "20210101")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PapirusDevelopmentTeam/papirus-icon-theme")
             (commit version)))
       (sha256
        (base32
         "0w6qg3zjhfvjg1gg5inranf8ianb4mrp0jm9qgi6hg87ig1rashs"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (delete 'build))))
    (native-inputs
     `(("gtk+:bin" ,gtk+ "bin")))
    (home-page "https://git.io/papirus-icon-theme")
    (synopsis "Fork of Paper icon theme with a lot of new icons and a few extras")
    (description "Papirus is a fork of the icon theme Paper with a lot of new icons
and a few extra features.")
    (license license:gpl3)))

(define-public gnome-shell-extension-appindicator
  (package
    (name "gnome-shell-extension-appindicator")
    (version "33")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/ubuntu/gnome-shell-extension-appindicator")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0qm77s080nbf4gqnfzpwp8a7jf7lliz6fxbsd3lasvrr11pgsk87"))
              (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." ,(string-append "share/gnome-shell/extensions/"
                              "appindicatorsupport@rgcjonas.gmail.com")))))
    (synopsis "Adds KStatusNotifierItem support to GNOME Shell")
    (description "This extension integrates Ubuntu AppIndicators
and KStatusNotifierItems (KDE's successor of the systray) into
GNOME Shell.")
    (home-page "https://github.com/ubuntu/gnome-shell-extension-appindicator/")
    (license license:gpl2+)))

(define-public gnome-shell-extension-clipboard-indicator
  (package
    (name "gnome-shell-extension-clipboard-indicator")
    (version "34")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/Tudmotu/"
                                        "gnome-shell-extension-clipboard-indicator.git"))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i00psc1ky70zljd14jzr627y7nd8xwnwrh4xpajl1f6djabh12s"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove pre-compiled settings schemas and translations from
               ;; source, as they are generated as part of build. Upstream
               ;; includes them for people who want to run the software
               ;; directly from source tree.
               '(begin (delete-file "schemas/gschemas.compiled")
                       (for-each delete-file (find-files "locale" "\\.mo$"))
                       #t))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "share/gnome-shell/extensions/clipboard-indicator@tudmotu.com"
          #:include-regexp ("\\.css$" "\\.compiled$" "\\.js(on)?$" "\\.mo$" "\\.xml$")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'compile-schemas
           (lambda _
             (with-directory-excursion "schemas"
               (invoke "glib-compile-schemas" "."))
             #t))
         (add-before 'install 'compile-locales
           (lambda _ (invoke "./compile-locales.sh")
                   #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")))       ; for glib-compile-schemas
    (home-page "https://github.com/Tudmotu/gnome-shell-extension-clipboard-indicator")
    (synopsis "Clipboard manager extension for GNOME Shell")
    (description "Clipboard Indicator is a clipboard manager for GNOME Shell
that caches clipboard history.")
    (license license:expat)))

(define-public gnome-shell-extension-topicons-redux
  (package
    (name "gnome-shell-extension-topicons-redux")
    (version "6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/pop-planet/TopIcons-Redux.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dli9xb545n3xlj6q4wl0y5gzkm903zs47p8fiq71pdvbr6v38rj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib" ,glib "bin")))
    (arguments
     `(#:tests? #f                      ;no test defined in the project
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make"
                       "install"
                       (string-append
                        "INSTALL_PATH="
                        out
                        "/share/gnome-shell/extensions"))))))))
    (home-page "https://gitlab.com/pop-planet/TopIcons-Redux")
    (synopsis "Display legacy tray icons in the GNOME Shell top panel")
    (description "Many applications, such as chat clients, downloaders, and
some media players, are meant to run long-term in the background even after you
close their window.  These applications remain accessible by adding an icon to
the GNOME Shell Legacy Tray.  However, the Legacy Tray was removed in GNOME
3.26.  TopIcons Redux brings those icons back into the top panel so that it's
easier to keep track of applications running in the background.")
    (license license:gpl2+)))

(define-public gnome-shell-extension-dash-to-dock
  (package
    (name "gnome-shell-extension-dash-to-dock")
    (version "67")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/micheleg/dash-to-dock")
                    (commit (string-append "extensions.gnome.org-v"
                                           version))))
              (sha256
               (base32
                "1746xm0iyvyzj6m3pvjx11smh9w1s7naz426ki0dlr5l7jh3mpy5"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags (list (string-append "INSTALLBASE="
                                         (assoc-ref %outputs "out")
                                         "/share/gnome-shell/extensions"))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)))
    (synopsis "Transforms GNOME's dash into a dock")
    (description "This extension moves the dash out of the
overview, transforming it into a dock for easier application launching and
faster window switching.")
    (home-page "https://micheleg.github.io/dash-to-dock/")
    (license license:gpl2+)))

(define-public gnome-shell-extension-gsconnect
  (package
    (name "gnome-shell-extension-gsconnect")
    ;; v33 is the last version to support GNOME 3.34
    (version "33")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/andyholmes"
                                        "/gnome-shell-extension-gsconnect.git"))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q03axhn75i864vgmd6myhmgwrmnpf01gsd1wl0di5x9q8mic2zn"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (name+version (strip-store-file-name out))
              (gschema-dir (string-append out
                                          "/share/gsettings-schemas/"
                                          name+version
                                          "/glib-2.0/schemas"))
              (gnome-shell (assoc-ref %build-inputs "gnome-shell"))
              (openssh (assoc-ref %build-inputs "openssh"))
              (openssl (assoc-ref %build-inputs "openssl")))
         (list
          (string-append "-Dgnome_shell_libdir=" gnome-shell "/lib")
          (string-append "-Dgsettings_schemadir=" gschema-dir)
          (string-append "-Dopenssl_path=" openssl "/bin/openssl")
          (string-append "-Dsshadd_path=" openssh "/bin/ssh-add")
          (string-append "-Dsshkeygen_path=" openssh "/bin/ssh-keygen")
          (string-append "-Dsession_bus_services_dir=" out "/share/dbus-1/services")
          "-Dpost_install=true"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((glib (assoc-ref inputs "glib:bin"))
                    (gapplication (string-append glib "/bin/gapplication"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (substitute* "data/org.gnome.Shell.Extensions.GSConnect.desktop"
                 (("gapplication") gapplication))
               (for-each
                (lambda (file)
                  (substitute* file
                    (("'use strict';")
                     (string-append "'use strict';\n\n"
                                    "'" gi-typelib-path "'.split(':').forEach("
                                    "path => imports.gi.GIRepository.Repository."
                                    "prepend_search_path(path));"))))
                '("src/extension.js" "src/prefs.js"))
               #t)))
         (add-after 'install 'wrap-daemons
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (service-dir
                     (string-append out "/share/gnome-shell/extensions"
                                    "/gsconnect@andyholmes.github.io/service"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append service-dir "/daemon.js")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))
               #t))))))
    (inputs
     `(("at-spi2-core" ,at-spi2-core)
       ("caribou" ,caribou)
       ("evolution-data-server" ,evolution-data-server)
       ("gjs" ,gjs)
       ("glib" ,glib)
       ("glib:bin" ,glib "bin")
       ("gsound" ,gsound)
       ("gnome-shell" ,gnome-shell)
       ("gtk+" ,gtk+)
       ("nautilus" ,nautilus)
       ("openssh" ,openssh)
       ("openssl" ,openssl)
       ("python-nautilus" ,python-nautilus)
       ("python-pygobject" ,python-pygobject)
       ("upower" ,upower)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection)
       ("libxml2" ,libxml2)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/andyholmes/gnome-shell-extension-gsconnect/wiki")
    (synopsis "Connect GNOME Shell with your Android phone")
    (description "GSConnect is a complete implementation of KDE Connect
especially for GNOME Shell, allowing devices to securely share content, like
notifications or files, and other features like SMS messaging and remote
control.")
    (license license:gpl2)))

(define-public gnome-shell-extension-hide-app-icon
  (let ((commit "4188aa5f4ba24901a053a0c3eb0d83baa8625eab")
        (revision "0"))
    (package
      (name "gnome-shell-extension-hide-app-icon")
      (version (git-version "2.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (string-append "https://github.com/michael-rapp"
                                   "/gnome-shell-extension-hide-app-icon.git"))
               (commit commit)))
         (sha256
          (base32
           "1i28n4bz6wrhn07vpxkr6l1ljyn7g8frp5xrr11z3z32h2hxxcd6"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f                ; no test target
         #:make-flags (list (string-append "EXTENSIONS_DIR="
                                           (assoc-ref %outputs "out")
                                           "/share/gnome-shell/extensions"))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)      ; no configure script
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (pre "/share/gnome-shell/extensions/")
                     (dir "hide-app-icon@mrapp.sourceforge.com"))
                 (copy-recursively dir (string-append out pre dir))
                 #t))))))
      (native-inputs
       `(("glib" ,glib "bin")
         ("intltool" ,intltool)))
      (propagated-inputs
       `(("glib" ,glib)))
      (synopsis "Hide app icon from GNOME's panel")
      (description "This extension hides the icon and/or title of the
currently focused application in the top panel of the GNOME shell.")
      (home-page
       "https://github.com/michael-rapp/gnome-shell-extension-hide-app-icon/")
      (license
        ;; README.md and LICENSE.txt disagree -- the former claims v3, the
        ;; latter v2.  No mention of "or later" in either place or in the code.
        (list license:gpl2
              license:gpl3)))))

(define-public gnome-shell-extension-dash-to-panel
  (package
    (name "gnome-shell-extension-dash-to-panel")
    (version "38")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/home-sweet-gnome/dash-to-panel")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1kvybb49l1vf0fvh8d0c6xkwnry8m330scamf5x40y63d4i213j1"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "INSTALLBASE="
                                         (assoc-ref %outputs "out")
                                         "/share/gnome-shell/extensions")
                          (string-append "VERSION="
                                         ,(package-version
                                           gnome-shell-extension-dash-to-panel)))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glib" ,glib)
       ("glib" ,glib "bin")))
    (synopsis "Icon taskbar for GNOME Shell")
    (description "This extension moves the dash into the gnome main
panel so that the application launchers and system tray are combined
into a single panel, similar to that found in KDE Plasma and Windows 7+.")
    (home-page "https://github.com/home-sweet-gnome/dash-to-panel/")
    (license license:gpl2+)))

(define-public gnome-shell-extension-noannoyance
  (package
    (name "gnome-shell-extension-noannoyance")
    (version "5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/BjoernDaase/noannoyance")
                    (commit "e37b5b3c31f577b4698bc6659bc9fec5ea9ac5d4")))
              (sha256
               (base32
                "0fa8l3xlh8kbq07y4385wpb908zm6x53z81q16xlmin97dln32hh"))
              (file-name (git-file-name name version))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "share/gnome-shell/extensions/noannoyance@daase.net"))))
    (synopsis "Remove 'Window is ready' annotation")
    (description "One of the many extensions that remove this message.
It uses ES6 syntax and claims to be more actively maintained than others.")
    (home-page "https://extensions.gnome.org/extension/2182/noannoyance/")
    (license license:gpl2)))

(define-public gnome-shell-extension-paperwm
  (package
    (name "gnome-shell-extension-paperwm")
    (version "36.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/paperwm/PaperWM")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ssnabwxrns36c61ppspjkr9i3qifv08pf2jpwl7cjv3pvyn4kly"))
              (snippet
               '(begin (delete-file "schemas/gschemas.compiled")))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "share/gnome-shell/extensions/paperwm@hedning:matrix.org"
          #:include-regexp ("\\.js(on)?$" "\\.css$" "\\.ui$" "\\.png$"
                            "\\.xml$" "\\.compiled$")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'compile-schemas
           (lambda _
             (with-directory-excursion "schemas"
               (invoke "make"))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin"))) ; for glib-compile-schemas
    (home-page "https://github.com/paperwm/PaperWM")
    (synopsis "Tiled scrollable window management for GNOME Shell")
    (description "PaperWM is an experimental GNOME Shell extension providing
scrollable tiling of windows and per monitor workspaces.  It's inspired by paper
notebooks and tiling window managers.")
    (license license:gpl3)))

(define-public arc-theme
  (package
    (name "arc-theme")
    (version "20201013")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jnsh/arc-theme")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x2l1mwjx68dwf3jb1i90c1q8nqsl1wf2zggcn8im6590k5yv39s"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--disable-cinnamon")
       #:phases
       (modify-phases %standard-phases
         ;; autogen.sh calls configure at the end of the script.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi")))
         (add-before 'build 'set-home   ;placate Inkscape
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("glib" ,glib "bin")             ; for glib-compile-resources
       ("gnome-shell" ,gnome-shell)
       ("gtk+" ,gtk+)
       ("inkscape" ,inkscape)
       ("optipng" ,optipng)
       ("pkg-config" ,pkg-config)
       ("sassc" ,sassc/libsass-3.5)))
    (synopsis "A flat GTK+ theme with transparent elements")
    (description "Arc is a flat theme with transparent elements for GTK 3, GTK
2, and GNOME Shell which supports GTK 3 and GTK 2 based desktop environments
like GNOME, Unity, Budgie, Pantheon, XFCE, Mate, etc.")
    (home-page "https://github.com/horst3180/arc-theme")
    ;; No "or later" language found.
    (license license:gpl3+)))

(define-public greybird-gtk-theme
  (package
    (name "greybird-gtk-theme")
    (version "3.22.13")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/shimmerproject/Greybird")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "154qawiga792iimkpk3a6q8f4gm4r158wmsagkbqqbhj33kxgxhg"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gtk+" ,gtk+)
       ("glib:bin" ,glib "bin")         ; for "glib-compile-resources"
       ("librsvg" ,librsvg)
       ("pkg-config" ,pkg-config)
       ("ruby-sass" ,ruby-sass)
       ("sassc" ,sassc)))
    (home-page "https://shimmerproject.org/")
    (synopsis "Grey GTK+ theme based on Bluebird")
    (description "Greybird is a grey derivative of the Bluebird theme by the
Shimmer Project.  It supports GNOME, Unity, and Xfce.")
    (license (list license:gpl2+ license:cc-by-sa3.0))))

(define-public matcha-theme
  (package
    (name "matcha-theme")
    (version "2021-01-01")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/vinceliuice/Matcha-gtk-theme")
            (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1pa6ra87wlq0gwz4n03l6xv0pxiamr5dygycvppms8v6xyc2aa0r"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (source (assoc-ref %build-inputs "source"))
                (bash (assoc-ref %build-inputs "bash"))
                (coreutils (assoc-ref %build-inputs  "coreutils"))
                (themesdir (string-append out "/share/themes")))
           (setenv "PATH"
                   (string-append coreutils "/bin:"
                                  (string-append bash "/bin:")))
           (copy-recursively source (getcwd))
           (patch-shebang "install.sh")
           (mkdir-p themesdir)
           (invoke "./install.sh" "-d" themesdir)
           #t))))
    (inputs
     `(("gtk-engines" ,gtk-engines)))
    (native-inputs
     `(("bash" ,bash)
       ("coreutils" ,coreutils)))
    (synopsis "Flat design theme for GTK 3, GTK 2 and GNOME-Shell")
    (description "Matcha is a flat Design theme for GTK 3, GTK 2 and
Gnome-Shell which supports GTK 3 and GTK 2 based desktop environments
like Gnome, Unity, Budgie, Pantheon, XFCE, Mate and others.")
    (home-page "https://github.com/vinceliuice/matcha")
    (license license:gpl3+)))

(define-public materia-theme
  (package
    (name "materia-theme")
    (version "20200916")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/nana-4/materia-theme")
            (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "0qaxxafsn5zd2ysgr0jyv5j73360mfdmxyd55askswlsfphssn74"))))
    (build-system meson-build-system)
    (native-inputs
     `(("gtk+" ,gtk+)
       ("sassc" ,sassc)))
    (home-page "https://github.com/nana-4/materia-theme")
    (synopsis "Material Design theme for a wide range of environments")
    (description "Materia is a Material Design theme for GNOME/GTK based
desktop environments.  It supports GTK 2, GTK 3, GNOME Shell, Budgie,
Cinnamon, MATE, Unity, Xfce, LightDM, GDM, Chrome theme, etc.")
    (license license:gpl2+)))

(define-public numix-gtk-theme
  (package
    (name "numix-gtk-theme")
    (version "2.6.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/numixproject/numix-gtk-theme")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12mw0kr0kkvg395qlbsvkvaqccr90cmxw5rrsl236zh43kj8grb7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list (string-append "INSTALL_DIR="
                            (assoc-ref %outputs "out")
                            "/share/themes/Numix"))
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))             ; no configure script
    (native-inputs
     `(("glib:bin" ,glib "bin")             ; for glib-compile-schemas
       ("gnome-shell" ,gnome-shell)
       ("gtk+" ,gtk+)
       ("xmllint" ,libxml2)
       ("ruby-sass" ,ruby-sass)))
    (synopsis "Flat theme with light and dark elements")
    (description "Numix is a modern flat theme with a combination of light and
dark elements.  It supports GNOME, Unity, Xfce, and Openbox.")
    (home-page "https://numixproject.github.io")
    (license license:gpl3+)))

(define-public numix-theme
  (deprecated-package "numix-theme" numix-gtk-theme))

(define-public orchis-theme
  (package
    (name "orchis-theme")
    (version "2021-02-28")
    (source
      (origin
        (method git-fetch)
        (uri
          (git-reference
            (url "https://github.com/vinceliuice/Orchis-theme")
            (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
           "1qp3phiza93qllrjm5xjjca5b7l2sbng8c382khy9m97grxvcq0y"))
        (modules '((guix build utils)
                   (ice-9 regex)
                   (srfi srfi-26)))
        (snippet
         '(begin
            (for-each
             (lambda (f)
               (let* ((r (make-regexp "\\.scss"))
                      (f* (regexp-substitute #f (regexp-exec r f) 'pre ".css")))
                 (if (file-exists? f*)
                     (delete-file f*))))
             (find-files "." ".*\\.scss"))
            #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list
                          "--dest" (string-append
                                    (assoc-ref %outputs "out")
                                    "/share/themes")
                          "--theme" "all"
                          "--radio-color")
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (replace 'build (lambda _ (invoke "./parse-sass.sh")))
         (replace 'install
           (lambda* (#:key configure-flags #:allow-other-keys)
             (mkdir-p
              (cadr (or (member "--dest" configure-flags)
                        (member "-d" configure-flags))))
             (apply invoke "./install.sh" configure-flags)
             #t)))))
    (inputs
     `(("gtk-engines" ,gtk-engines)))
    (native-inputs
     `(;("coreutils" ,coreutils)
       ("gtk+" ,gtk+)
       ("sassc" ,sassc)))
    (home-page "https://github.com/vinceliuice/Orchis-theme")
    (synopsis "Material Design theme for a wide range of environments")
    (description "Orchis is a Material Design them for GNOME/GTK based
desktop environments.  It is based on materia-theme and adds more color
variants.")
    (license (list license:gpl3            ; According to COPYING.
                   license:lgpl2.1         ; Some style sheets.
                   license:cc-by-sa4.0)))) ; Some icons

(define-public markets
  (package
    (name "markets")
    (version "0.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/bitstower/markets")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1jzv74l2jkdiqy1hp0ww5yla50dmrvjw7fgkmb26ynblr1nb3rrb"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("gtk-update-icon-cache") "true"))
             #t))
         (add-after 'unpack 'skip-update-desktop-database
           ;; Don't update desktop file database.
           (lambda _
             (substitute* "build-aux/meson/postinstall.py"
               (("update-desktop-database") "true"))
             #t)))))
    (inputs
     `(("gtk3" ,gtk+)
       ("gettext" ,gettext-minimal)
       ("libgee" ,libgee)
       ("libhandy0" ,libhandy-0.0)
       ("libsoup" ,libsoup)
       ("json-glib" ,json-glib)
       ("vala" ,vala)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin"))) ; for 'glib-compile-resources'
    (home-page "https://github.com/bitstower/markets")
    (synopsis "Stock, currency and cryptocurrency tracker")
    (description
     "Markets is a GTK application that displays financial data, helping users
track stocks, currencies and cryptocurrencies.")
    (license license:gpl3)))

(define-public vala-language-server
  (package
    (name "vala-language-server")
    ;; Note to maintainer: VLS must be built with a Vala toolchain the same
    ;; version or newer. Therefore when you update this package you may need
    ;; to update Vala too.
    (version "0.48.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/benwaffle/vala-language-server")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "12k095052jkvbiyz8gzkj6w7r7p16d5m18fyikl48yvh5nln8fw0"))))
    (build-system meson-build-system)
    (arguments '(#:glib-or-gtk? #t))
    (inputs
     `(("glib" ,glib)
       ("json-glib" ,json-glib)
       ("jsonrpc-glib" ,jsonrpc-glib)
       ("libgee" ,libgee)
       ("vala" ,vala-0.50)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/benwaffle/vala-language-server")
    (synopsis "Language server for Vala")
    (description "The Vala language server is an implementation of the Vala
language specification for the Language Server Protocol (LSP).  This tool is
used in text editing environments to provide a complete and integrated
feature-set for programming Vala effectively.")
    (license license:lgpl2.1+)))

(define-public nordic-theme
  (let ((commit "07d764c5ebd5706e73d2e573f1a983e37b318915")
	(revision "0"))
  (package
   (name "nordic-theme")
   (version (git-version "1.9.0" revision commit))
   (source
     (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/EliverLara/Nordic")
             (commit commit)))
     (sha256
       (base32
         "0y2s9d6h1b195s6afp1gb5rb1plfslkpbw2brd30a9d66wfvsqk0"))
     (file-name (git-file-name name version))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      `(("." "share/themes/nord"
         #:exclude ("README.md" "LICENSE" "Art/" "package.json"
                    "package-lock.json" "Gulpfile.js")))))
   (home-page "https://github.com/EliverLara/Nordic")
   (synopsis "Dark Gtk3.20+ theme using the Nord color pallete")
   (description "Nordic is a Gtk3.20+ theme created using the Nord color
pallete.")
   (license license:gpl3))))

(define-public tiramisu
  (let ((commit "8eb946dae0e2f98d3850d89e1bb535640e8c3266")
        (revision "0"))
    (package
      (name "tiramisu")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Sweets/tiramisu")
                      (commit commit)))
                (sha256
                 (base32
                  "0wz2r8369d40vnxswknx0zxzbs03gzv0nc8al4g0ffg972p15j25"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'check)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "tiramisu" (string-append out "/bin"))
                 #t))))
         #:make-flags
         (list (string-append "CC=" ,(cc-for-target)))))
      (inputs
       `(("glib" ,glib)))
      (native-inputs
       `(("pkg-config" ,pkg-config)))
      (home-page "https://github.com/Sweets/tiramisu")
      (synopsis "Desktop notifications, the UNIX way")
      (description "tiramisu is a notification daemon based on dunst that
outputs notifications to STDOUT in order to allow the user to process
notifications any way they prefer.")
      (license license:expat))))
