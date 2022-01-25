;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
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

(define-module (gnu packages syndication)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public cawbird
  (package
    (name "cawbird")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/IBBoard/cawbird")
             (commit (string-append "v"version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17575cp5qcgsqf37y3xqg3vr6l2j8bbbkmy2c1l185rxghfacida"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       ;; Cawbirds's default key and secret for OAuth process with twitter.
       (list
        "-Dconsumer_key_base64=VmY5dG9yRFcyWk93MzJEZmhVdEk5Y3NMOA=="
        "-Dconsumer_secret_base64=MThCRXIxbWRESDQ2Y0podzVtVU13SGUyVGlCRXhPb3BFRHhGYlB6ZkpybG5GdXZaSjI=")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; These tests require networking.
             (substitute* "tests/meson.build"
               (("[ \t]*.*avatardownload.*$") "")
               (("[ \t]*.*filters.*$") "")
               (("[ \t]*.*friends.*$") "")
               (("[ \t]*.*inlinemediadownloader.*$") "")
               (("[ \t]*.*tweetparsing.*$") "")
               (("[ \t]*.*usercounter.*$") ""))))
         (delete 'check)
         (add-after 'install 'custom-check
           (lambda* (#:key outputs tests? #:allow-other-keys)
             (when tests?
               ;; Tests require a running X server.
               (system "Xvfb :1 +extension GLX &")
               (setenv "DISPLAY" ":1")
               ;; Tests write to $HOME.
               (setenv "HOME" (getcwd))
               ;; Tests look for gsettings-schemas installed by the package.
               (setenv "XDG_DATA_DIRS"
                       (string-append (getenv "XDG_DATA_DIRS")
                                      ":" (assoc-ref outputs "out") "/share"))
               (invoke "meson" "test"))))
         (add-after 'glib-or-gtk-wrap 'wrap-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append bin "cawbird")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("glib" ,glib)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gspell" ,gspell)
       ("gstreamer" ,gstreamer)
       ("gst-libav" ,gst-libav)
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("liboauth" ,liboauth)
       ("libsoup" ,libsoup)
       ("rest" ,rest)
       ("sqlite" ,sqlite)
       ("x11" ,libx11)))
    (propagated-inputs
     (list dconf))
    (synopsis "Client for Twitter")
    (description "Cawbird is a Twitter client built with GTK and Vala.
It supports all features except non-mention notifications, polls, threads and
cards.")
    (home-page "https://ibboard.co.uk/cawbird/")
    (license license:gpl3+)))

(define-public giara
  (package
    (name "giara")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/giara")
             (commit version)))
       (file-name (git-file-name name version))
       ;; To fix authentication while adding accounts.
       (patches (search-patches "giara-fix-login.patch"))
       (sha256
        (base32 "004qmkfrgd37axv0b6hfh6v7nx4pvy987k5yv4bmlmkj9sbqm6f9"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'glib-or-gtk-wrap 'wrap-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (lib (string-append out "/lib/python"
                                        ,(version-major+minor
                                          (package-version python))
                                        "/site-packages")))
               (wrap-program (string-append bin "giara")
                 `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH") ,lib))
                 `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH"))))))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("libhandy" ,libhandy)
       ("python" ,python)
       ("python-beautifulsoup" ,python-beautifulsoup4)
       ("python-dateutil" ,python-dateutil)
       ("python-mistune" ,python-mistune)
       ("python-pillow" ,python-pillow)
       ("python-praw" ,python-praw)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-requests" ,python-requests)
       ("webkitgtk" ,webkitgtk-with-libsoup2)))
    (propagated-inputs
     (list dconf))
    (synopsis "Client for Reddit")
    (description "Giara is a reddit app, built with Python, GTK and Handy.")
    (home-page "https://giara.gabmus.org/")
    (license license:gpl3+)))

(define-public newsboat
  (package
    (name "newsboat")
    (version "2.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://newsboat.org/releases/" version
                           "/newsboat-" version ".tar.xz"))
       (sha256
        (base32 "061w86jffyi49m4d9n974a3pd1svbw3azmh0qx8h2v7h0178791l"))))
    (build-system cargo-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("openssl" ,openssl)
       ("pkg-config" ,pkg-config)
       ;; For building documentation.
       ("asciidoctor" ,ruby-asciidoctor)))
    (inputs
     (list curl
           json-c
           libxml2
           ncurses
           stfl
           sqlite))
    (arguments
     `(#:modules ((guix build cargo-build-system)
                  (guix build utils)
                  ((guix build gnu-build-system) #:prefix gnu:))
       #:vendor-dir "vendor"
       #:install-source? #f
       #:cargo-inputs
       (("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-bitflags" ,rust-bitflags-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-curl-sys" ,rust-curl-sys-0.4)
        ("rust-cxx" ,rust-cxx-1)
        ("rust-fastrand" ,rust-fastrand-1)
        ("rust-gettext-rs" ,rust-gettext-rs-0.7)
        ("rust-lexopt" ,rust-lexopt-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-md5" ,rust-md5-0.7)
        ("rust-natord" ,rust-natord-1)
        ("rust-nom" ,rust-nom-7)
        ("rust-once-cell" ,rust-once-cell-1)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-url" ,rust-url-2)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-xdg" ,rust-xdg-2))
       #:cargo-development-inputs
       (("rust-cxx-build" ,rust-cxx-build-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-section-testing" ,rust-section-testing-0.0))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'dont-vendor-self
           (lambda* (#:key vendor-dir #:allow-other-keys)
             ;; Don't keep the whole tarball in the vendor directory
             (delete-file-recursively
               (string-append vendor-dir "/" ,name "-" ,version ".tar.xz"))))
         (add-after 'unpack 'patch-source
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("Cargo.lock") "")
               ;; Replace the prefix in the Makefile.
               (("/usr/local") (assoc-ref outputs "out")))))
         (replace 'build
           (assoc-ref gnu:%standard-phases 'build))
         (replace 'check
           (lambda args
             ((assoc-ref gnu:%standard-phases 'check)
              #:test-target "test")))
         (replace 'install
           (assoc-ref gnu:%standard-phases 'install)))))
    (native-search-paths
     ;; Newsboat respects CURL_CA_BUNDLE.
     (package-native-search-paths curl))
    (home-page "https://newsboat.org/")
    (synopsis "Text-mode RSS and Atom feed reader with podcast support")
    (description "Newsboat is a feed reader for @dfn{RSS} and @dfn{Atom}, XML
formats widely used to transmit, publish, and syndicate news or blog articles.
It's designed for use on text terminals, and to have a coherent and easy-to-use
interface that might look familiar to @command{mutt} or @command{slrn} users.

Newsboat supports OPML import/exports, HTML rendering, podcasts (with
@command{podboat}), off-line reading, searching and storing articles to your
file system, and many more features.")
    (properties '((release-monitoring-url . "https://newsboat.org/news.atom")))
    (license (list license:gpl2+        ; filter/*
                   license:expat))))    ; everything else

(define-public newsboat-2.13
  (package
    (inherit newsboat)
    (version "2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://newsboat.org/releases/" version
                           "/newsboat-" version ".tar.xz"))
       (sha256
        (base32
         "0pik1d98ydzqi6055vdbkjg5krwifbk2hy2f5jp5p1wcy2s16dn7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(,@(fold alist-delete (package-native-inputs newsboat)
               '("asciidoctor" "openssl"))
       ;; For building documentation.
       ("asciidoc" ,asciidoc)))
    (inputs
     (modify-inputs (package-inputs newsboat)
       (replace "json-c" json-c-0.13)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'build 'build-documentation
           (lambda _
             (invoke "make" "doc"))))
       #:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:test-target "test"))))

(define-public liferea
  (package
    (name "liferea")
    (version "1.13.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lwindolf/liferea/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g9463bvswsm899j6dfhslcg6np70m5wq143mjicr24zy8d17bm7"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
         "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'prepare-build-environment
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Workaround for https://github.com/lwindolf/liferea/issues/767.
             (setenv "WEBKIT_DISABLE_COMPOSITING_MODE" "1")))
         (add-after 'install 'wrap-gi-python
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (python-path       (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/liferea")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 `("GUIX_PYTHONPATH" ":" prefix (,python-path))))
             #t)))))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           intltool
           libtool
           pkg-config
           which))
    (inputs
     (list glib
           glib-networking
           gnome-keyring
           gsettings-desktop-schemas
           gstreamer
           json-glib
           libnotify
           libpeas
           libsecret
           libsoup-minimal-2
           libxml2
           libxslt
           pango
           python
           python-pycairo
           python-pygobject
           sqlite
           webkitgtk-with-libsoup2))
    (home-page "https://lzone.de/liferea/")
    (synopsis "News reader for GTK/GNOME")
    (description "Liferea is a desktop feed reader/news aggregator that
brings together all of the content from your favorite subscriptions into
a simple interface that makes it easy to organize and browse feeds.")
    (license license:gpl2+)))

(define-public rtv
  (package
    (name "rtv")
    (version "1.27.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rtv" version))
        (sha256
         (base32 "0hvw426y09l3yzwv2zkb9hifpfbg9wd1gg0y3z3pxcli6n3ii2wl"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-environment-variables
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HOME" (getcwd))
             (setenv "TERM" "linux")
             (setenv "TERMINFO"
                     (search-input-directory inputs "share/terminfo"))))
         ;; Loading this as a library requires a controlling terminal, etc.
         (delete 'sanity-check))
       #:tests? #f)) ; tests fail: _curses.error: nocbreak() returned ERR
    (propagated-inputs
     (list python-beautifulsoup4 python-decorator python-kitchen
           python-requests python-six))
    (native-inputs
     (list ncurses
           python-coveralls
           python-coverage
           python-mock
           python-pylint
           python-pytest
           python-vcrpy))
    (home-page "https://github.com/michael-lazar/rtv")
    (synopsis "Terminal viewer for Reddit (Reddit Terminal Viewer)")
    (description
     "RTV provides a text-based interface to view and interact with Reddit.")
    (license (list license:expat
                   license:gpl3+)))) ; rtv/packages/praw

(define-public tuir
  (package
    (name "tuir")
    (version "1.29.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "tuir" version))
        (sha256
         (base32
          "06xb030ibphbrz4nsxm8mh3g60ld8xfp6kc3j6vi1k4ls5s4h79i"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'sanity-check)         ; Tries to read environment variables.
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (when tests?
               (invoke "pytest")))))))
    (inputs
     (list python-beautifulsoup4 python-decorator python-kitchen
           python-requests python-six))
    (native-inputs
     (list python-coverage
           python-coveralls
           python-mock
           python-pylint
           python-pytest
           python-vcrpy))
    (home-page "https://gitlab.com/ajak/tuir")
    (synopsis "Terminal viewer for Reddit (Terminal UI for Reddit)")
    (description
     "Tuir provides a simple terminal viewer for Reddit (Terminal UI for Reddit).")
    (license (list license:expat
                   license:gpl3+))))    ; tuir/packages/praw

(define-public rawdog
  (package
    (name "rawdog")
    (version "2.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://offog.org/files/rawdog-"
                           version ".tar.gz"))
       (sha256
        (base32
         "18nyg19mwxyqdnykplkqmzb4n27vvrhvp639zai8f81gg9vdbsjp"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.7))
    (inputs
     (list python2-feedparser python2-pytidylib))
    (home-page "https://offog.org/code/rawdog/")
    (synopsis "RSS Aggregator Without Delusions Of Grandeur")
    (description
     "@command{rawdog} is a feed aggregator, capable of producing a personal
\"river of news\" or a public \"planet\" page.  It supports all common feed
formats, including all versions of RSS and Atom.")
    (license license:gpl2+)))

(define-public quiterss
  (package
    (name "quiterss")
    (version "0.19.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/QuiteRSS/quiterss")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cgvl67vhn5y7bj5gbjbgk26bhb0196bgrgsp3r5fmrislarj8s6"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* (find-files "." "\\.cpp$")
                    ;; Disable Google Analytics spyware by default,
                    ;; removing completely is not trivial.
                    (("settings\\.value\\(\"Settings/statisticsEnabled2\", true\\)")
                     "settings.value(\"Settings/statisticsEnabled2\", false)")
                    ;; Disable update check spyware by default, otherwise runs
                    ;; at every startup, nasty. Not needed on GNU Guix as a
                    ;; feature either way.
                    (("settings\\.value\\(\"Settings/updateCheckEnabled\", true\\)")
                     "settings.value(\"Settings/updateCheckEnabled\", false)"))
                  #t))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ;; no test suite
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "qmake" "CONFIG+=release"
                     (string-append "PREFIX="
                                    (assoc-ref outputs "out"))
                     (string-append "QMAKE_LRELEASE="
                                    (assoc-ref inputs "qttools")
                                    "/bin/lrelease")))))))
    (native-inputs
     (list pkg-config qttools))
    (inputs
     (list qtwebkit qtbase-5 qtmultimedia phonon sqlite))
    (home-page "https://quiterss.org/")
    (synopsis "RSS/Atom news feeds reader written on Qt/C++")
    (description "QuiteRSS is an RSS/Atom news feeds reader written on Qt/C++
that aims to be quite fast and comfortable to its user.")
    (license license:gpl3+)))

(define-public gfeeds
  (package
    (name "gfeeds")
    (version "0.16.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://gitlab.gnome.org/World/gfeeds/-/archive/" version
                "/gfeeds-" version ".tar.bz2"))
              (sha256
               (base32
                "05gwwzqfz29m477imd5vh84jfla1wnklwpc2sdxnqli72wg08fli"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-mpv-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gfeeds/confManager.py"
               (("mpv") (search-input-file inputs "/bin/mpv")))
             #t))
         (add-after 'install 'wrap-gfeeds
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append
                            (assoc-ref outputs "out") "/bin/gfeeds")
               `("PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
               `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH")))
               `("XDG_DATA_DIRS" ":" prefix (,(getenv "XDG_DATA_DIRS"))))
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     (list glib
           gsettings-desktop-schemas
           gtk+
           hicolor-icon-theme
           libhandy
           mpv
           python
           python-beautifulsoup4
           python-dateutil
           python-feedparser
           python-html5lib
           python-listparser
           python-lxml
           python-pillow
           python-pygments
           python-pytz
           python-readability
           python-requests
           webkitgtk
           python-pygobject))
    (home-page "https://gfeeds.gabmus.org/")
    (synopsis "Easy-to-use GTK+ RSS/Atom feed reader")
    (description "Feeds is an RSS/Atom feed reader made with GTK+
and it has an easy-to-use graphical user interface.")
    (license license:gpl3+)))
