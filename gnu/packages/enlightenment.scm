;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015 Daniel Pimentel <d4n1@member.fsf.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Timo Eisenmann <eisenmann@fn.de>
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

(define-module (gnu packages enlightenment)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public efl
  (package
    (name "efl")
    (version "1.23.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.enlightenment.org/rel/libs/efl/efl-"
                    version ".tar.xz"))
              (sha256
               (base32
                "00b9lp3h65254kdb1ys15fv7p3ln7qsvf15jkw4kli5ymagadkjk"))))
    (build-system meson-build-system)
    (native-inputs
     `(("check" ,check)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("giflib" ,giflib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("ibus" ,ibus)
       ("mesa" ,mesa)
       ("libraw" ,libraw)
       ("librsvg" ,librsvg)
       ("libspectre" ,libspectre)
       ("libxau" ,libxau)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxdamage" ,libxdamage)
       ("libxdmcp" ,libxdmcp)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxfixes" ,libxfixes)
       ("libxinerama" ,libxinerama)
       ("libxp" ,libxp)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxss" ,libxscrnsaver)
       ("libxtst" ,libxtst)
       ("poppler" ,poppler)
       ("wayland-protocols" ,wayland-protocols)))
    (propagated-inputs
     ;; All these inputs are in package config files in section
     ;; Requires.private.
     `(("avahi" ,avahi)
       ("bullet" ,bullet)
       ("dbus" ,dbus)
       ("elogind" ,elogind)
       ("eudev" ,eudev)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("glib" ,glib)
       ("harfbuzz" ,harfbuzz)
       ("luajit" ,luajit)
       ("libinput" ,libinput-minimal)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libsndfile" ,libsndfile)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("libx11" ,libx11)
       ("libxkbcommon" ,libxkbcommon)
       ("lz4" ,lz4)
       ("openssl" ,openssl)
       ("pulseaudio" ,pulseaudio)
       ("util-linux" ,util-linux)
       ("wayland" ,wayland)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags '("-Dsystemd=false"
                           "-Dembedded-lz4=false"
                           "-Devas-loaders-disabler=json"
                           "-Dbuild-examples=false"
                           ;(string-append "-Ddictionaries-hyphen-dir="
                           ;               (assoc-ref %build-inputs "hyphen")
                           ;               "/share/hyphen")
                           "-Delogind=true"
                           "-Dnetwork-backend=connman"
                           ,@(match (%current-system)
                               ("armhf-linux"
                                '("-opengl=es-egl"))
                               (_
                                '("-Dopengl=full")))
                           ;; for wayland
                           "-Dwl-deprecated=true" ; ecore_wayland
                           "-Ddrm-deprecated=true" ; ecore_drm
                           "-Dwl=true"
                           "-Ddrm=true")
       #:tests? #f ; Many tests fail due to timeouts and network requests.
       #:phases
       (modify-phases %standard-phases
         ;; If we don't hardcode the location of libcurl.so then we
         ;; have to wrap the outputs of efl's dependencies in curl.
         (add-after 'unpack 'hardcode-libcurl-location
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((curl (assoc-ref inputs "curl"))
                    (lib  (string-append curl "/lib/")))
               (substitute* "src/lib/ecore_con/ecore_con_url_curl.c"
                 (("libcurl.so.?" libcurl) ; libcurl.so.[45]
                  (string-append lib libcurl)))
               #t)))
         (add-after 'unpack 'fix-install-paths
           (lambda _
             (substitute* "dbus-services/meson.build"
               (("install_dir.*")
                "install_dir: join_paths(dir_data, 'dbus-1', 'services'))\n"))
             (substitute* "src/tests/elementary/meson.build"
               (("dir_data") "meson.source_root(), 'test-output'"))
             #t))
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp") #t)))))
    (home-page "https://www.enlightenment.org/about-efl")
    (synopsis "Enlightenment Foundation Libraries")
    (description
     "Enlightenment Foundation Libraries is a set of libraries developed
for Enlightenment.  Libraries covers data serialization, wide support for
graphics rendering, UI layout and themes, interaction with OS, access to
removable devices or support for multimedia.")
    ;; Different parts are under different licenses.
    (license (list license:bsd-2 license:lgpl2.1 license:zlib))))

(define-public terminology
  (package
    (name "terminology")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://download.enlightenment.org/rel/apps/"
                              "terminology/terminology-" version ".tar.xz"))
              (sha256
               (base32
                "0xxx4xyhis6fy3frgb34ip0aj0kc4zashf60gzbxmq5gadbb0p5r"))
              (modules '((guix build utils)))
              ;; Remove the bundled fonts.
              (snippet
               '(begin
                  (delete-file-recursively "data/fonts")
                  (substitute* "data/meson.build"
                    (("subdir\\('fonts'\\)") ""))
                  #t))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags (list "-Dtests=true"
                               (string-append "-Dedje-cc="
                                              (assoc-ref %build-inputs "efl")
                                              "/bin/edje_cc"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp") #t))
         (replace 'check
           (lambda _
             (with-directory-excursion
               (string-append "../" ,name "-" ,version "/tests")
               (invoke "sh" "run_tests.sh" "--verbose"
                       "-t" "../../build/src/bin/tytest"))))
         (add-after 'install 'remove-test-binary
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This file is not meant to be installed.
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/bin/tytest"))
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("efl" ,efl)))
    (home-page "https://www.enlightenment.org/about-terminology")
    (synopsis "Powerful terminal emulator based on EFL")
    (description
     "Terminology is fast and feature rich terminal emulator.  It is solely
based on Enlightenment Foundation Libraries.  It supports multiple tabs, UTF-8,
URL and local path detection, themes, popup based content viewer for non-text
contents and more.")
    (license license:bsd-2)))

(define-public rage
  (package
    (name "rage")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://download.enlightenment.org/rel/apps/rage/rage-"
                version ".tar.xz"))
              (sha256
               (base32
                "04fdk23bbgvni212zrfy4ndg7vmshbsjgicrhckdvhay87pk9i75"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp") #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("efl" ,efl)))
    (home-page "https://www.enlightenment.org/about-rage")
    (synopsis "Video and audio player based on EFL")
    (description
     "Rage is a video and audio player written with Enlightenment Foundation
Libraries with some extra bells and whistles.")
    (license license:bsd-2)))

(define-public enlightenment
  (package
    (name "enlightenment")
    (version "0.23.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://download.enlightenment.org/rel/apps/"
                              "enlightenment/enlightenment-" version ".tar.xz"))
              (sha256
               (base32
                "0d1cyl07w9pvi2pf029kablazks2q9aislzl46b6fq5m1465jc75"))
              (patches (search-patches "enlightenment-fix-setuid-path.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dsystemd=false")
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap) ; We don't want to run the autogen script.
         (add-after 'unpack 'fix-dot-desktop-creation
           (lambda _
             (substitute* "data/session/meson.build"
               (("HAVE_WAYLAND'.*") "HAVE_WAYLAND') == true\n"))
             #t))
         (add-before 'configure 'set-system-actions
           (lambda* (#:key inputs #:allow-other-keys)
            (setenv "HOME" "/tmp")
             (let ((xkeyboard (assoc-ref inputs "xkeyboard-config"))
                   (setxkbmap (assoc-ref inputs "setxkbmap"))
                   (utils     (assoc-ref inputs "util-linux"))
                   (libc      (assoc-ref inputs "libc"))
                   (bluez     (assoc-ref inputs "bluez"))
                   (bc        (assoc-ref inputs "bc"))
                   (efl       (assoc-ref inputs "efl")))
               ;; We need to patch the path to 'base.lst' to be able
               ;; to switch the keyboard layout in E.
               (substitute* (list "src/modules/xkbswitch/e_mod_parse.c"
                                  "src/modules/wizard/page_011.c")
                 (("/usr/share/X11/xkb/rules/xorg.lst")
                  (string-append xkeyboard
                                 "/share/X11/xkb/rules/base.lst")))
               (substitute* "src/bin/e_xkb.c"
                 (("\"setxkbmap \"")
                  (string-append "\"" setxkbmap "/bin/setxkbmap \"")))
               (substitute* (list "src/bin/e_intl.c"
                                  "src/modules/conf_intl/e_int_config_intl.c"
                                  "src/modules/wizard/page_010.c")
                 (("locale -a") (string-append libc "/bin/locale -a")))
               (substitute* "src/bin/e_import_config_dialog.c"
                 (("%s/edje_cc -v %s %s %s\", e_prefix_bin_get\\(\\)")
                  (string-append efl "/bin/edje_cc -v %s %s %s\"")))
               (substitute* "src/modules/everything/evry_plug_apps.c"
                 (("/usr/bin/") ""))
               (substitute* "src/modules/everything/evry_plug_calc.c"
                 (("bc -l") (string-append bc "/bin/bc -l")))
               (substitute* "data/etc/meson.build"
                 (("/bin/mount") (string-append utils "/bin/mount"))
                 (("/bin/umount") (string-append utils "/bin/umount"))
                 (("/usr/bin/eject") (string-append utils "/bin/eject"))
                 (("/usr/bin/l2ping") (string-append bluez "/bin/l2ling"))
                 (("/bin/rfkill") (string-append utils "/sbin/rfkill"))
                 (("SUSPEND   = ''") "SUSPEND   = '/run/current-system/profile/bin/loginctl suspend'")
                 (("HIBERNATE = ''") "HIBERNATE = '/run/current-system/profile/bin/loginctl hibernate'")
                 (("/sbin/shutdown -h now") "/run/current-system/profile/bin/loginctl poweroff now")
                 (("/sbin/shutdown -r now") "/run/current-system/profile/bin/loginctl reboot now"))
               #t))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("util-linux" ,util-linux)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bc" ,bc)
       ("bluez" ,bluez)
       ("dbus" ,dbus)
       ("efl" ,efl)
       ("freetype" ,freetype)
       ("libxcb" ,libxcb)
       ("libxext" ,libxext)
       ("linux-pam" ,linux-pam)
       ("puleseaudio" ,pulseaudio)
       ("setxkbmap" ,setxkbmap)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xkeyboard-config" ,xkeyboard-config)))
    (home-page "https://www.enlightenment.org/about-enlightenment")
    (synopsis "Lightweight desktop environment")
    (description
     "Enlightenment is resource friendly desktop environment with integrated
file manager, wide range of configuration options, plugin system allowing to
unload unused functionality, with support for touchscreen and suitable for
embedded systems.")
    (license license:bsd-2)))

(define-public enlightenment-wayland
  (package
    (inherit enlightenment)
    (name "enlightenment-wayland")
    (arguments
     (substitute-keyword-arguments (package-arguments enlightenment)
       ((#:configure-flags flags)
        `(cons* "-Dwl=true" ,flags))))
    (inputs
     `(("wayland-protocols" ,wayland-protocols)
       ("xorg-server-xwayland" ,xorg-server-xwayland)
       ,@(package-inputs enlightenment)))))

(define-public python-efl
  (package
    (name "python-efl")
    (version "1.23.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/bindings/"
                            "python/python-efl-" version ".tar.xz"))
        (sha256
         (base32
          "16yn6a1b9167nfmryyi44ma40m20ansfpwgrvqzfvwix7qaz9pib"))
        (modules '((guix build utils)))
        ;; Remove files generated by Cython
        (snippet
          '(begin
             (copy-file "efl/dbus_mainloop/e_dbus.c" "efl/dbus_mainloop/e_dbus.q")
             (for-each delete-file (find-files "efl" ".*\\.c$"))
             (delete-file "efl/eo/efl.eo_api.h")
             (copy-file "efl/dbus_mainloop/e_dbus.q" "efl/dbus_mainloop/e_dbus.c")
             (delete-file "efl/dbus_mainloop/e_dbus.q")
             #t))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (setenv "ENABLE_CYTHON" "1")
             (invoke "python" "setup.py" "build")))
        (add-before 'build 'set-flags
          (lambda _
            (setenv "CFLAGS"
                    (string-append "-I" (assoc-ref %build-inputs "python-dbus")
                                   "/include/dbus-1.0"))
            #t))
        (add-before 'check 'set-environment
          (lambda _
            ;; Some tests require write access to HOME.
            (setenv "HOME" "/tmp")
            ;; These tests try to connect to the internet.
            (delete-file "tests/ecore/test_09_file_download.py")
            (delete-file "tests/ecore/test_11_con.py")
            #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-cython" ,python-cython)))
    (inputs
     `(("efl" ,efl)
       ("python-dbus" ,python-dbus)))
    (home-page "https://www.enlightenment.org/")
    (synopsis "Python bindings for EFL")
    (description
     "PYTHON-EFL are the python bindings for the whole Enlightenment Foundation
Libraries stack (eo, evas, ecore, edje, emotion, ethumb and elementary).")
    (license license:lgpl3)))

(define-public python2-efl
  (package-with-python2 python-efl))

(define-public edi
  (package
    (name "edi")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/apps/edi/"
                            name "-" version ".tar.xz"))
        (sha256
         (base32
          "0iqkah327ms5m7k054hcik2l9v68i4mg9yy52brprfqpd5jk7pw8"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp") #t)))
       #:tests? #f)) ; tests require running dbus service
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("clang" ,clang)
       ("efl" ,efl)))
    (home-page "https://www.enlightenment.org/about-edi")
    (synopsis "Development environment for Enlightenment")
    (description "EDI is a development environment designed for and built using
the EFL.  It's aim is to create a new, native development environment for Linux
that tries to lower the barrier to getting involved in Enlightenment development
and in creating applications based on the Enlightenment Foundation Library suite.")
    (license (list license:public-domain ; data/extra/skeleton
                   license:gpl2))))      ; edi

(define-public lekha
  (package
    (name "lekha")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Lekha" version))
              (sha256
               (base32
                "0zr6i74ik58pbzrd7r9l7sawqbdv0r2c1a9927qkqzwga27x8j15"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-data-location
           (lambda _ (substitute* "setup.py"
                       (("'/usr/")"'"))
             #t)))))
    (propagated-inputs
     `(("python2-efl" ,python2-efl)
       ("python2-pypdf2" ,python2-pypdf2)
       ("python2-pyxdg" ,python2-pyxdg)))
    (synopsis "Simple PDF viewer")
    (description
     "Simple PDF viewer based on the Enlightenment Foundation Libraries.")
    (home-page "https://github.com/kaihu/lekha")
    (license license:gpl3+)))

(define-public ephoto
  (package
    (name "ephoto")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.enlightenment.org/rel/"
                           "apps/ephoto/ephoto-" version ".tar.xz"))
       (sha256
        (base32 "1q7v9abjp9jrs08xc7pqaac64yzax24dk1snjb9rciarzzh3mlzy"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp") #t)))))
    (native-inputs
     `(("check" ,check)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("efl" ,efl)))
    (home-page "https://smhouston.us/projects/ephoto/")
    (synopsis "EFL image viewer/editor/manipulator/slideshow creator")
    (description "Ephoto is an image viewer and editor written using the
@dfn{Enlightenment Foundation Libraries} (EFL).  It focuses on simplicity and
ease of use, while taking advantage of the speed and small footprint the EFL
provide.

Ephoto’s features include:
@enumerate
@item Browsing the file system and displaying images in an easy-to-use grid view.
@item Browsing images in a single image view format.
@item Viewing images in a slideshow.
@item Editing your image with features such as cropping, auto enhance,
blurring, sharpening, brightness/contrast/gamma adjustments, hue/saturation/value
adjustments, and color level adjustment.
@item Applying artistic filters to your image such as black and white and old
photo.
@item Drag And Drop along with file operations to easily maintain your photo
directories.
@end enumerate\n")
    (license (list
               license:bsd-2 ; Ephoto's thumbnailing code
               license:bsd-3))))

(define-public evisum
  (package
    (name "evisum")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/apps/"
                            "evisum/evisum-" version ".tar.xz"))
        (sha256
         (base32
          "1rg3kri6j8nmab0kdljnmcc096c8ibgwzvbhqr0b25xpmrq8bcac"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f   ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure phase
         (add-after 'unpack 'set-environmental-variables
           (lambda _ (setenv "CC" (which "gcc")) #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("efl" ,efl)
       ("perl" ,perl)))
    (home-page "https://www.enlightenment.org")
    (synopsis "EFL process viewer")
    (description
     "This is a process monitor and system monitor using the
@dfn{Enlightenment Foundation Libraries} (EFL).")
    (license license:bsd-2)))
