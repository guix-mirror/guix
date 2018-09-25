;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages ibus)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages anthy)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg))

(define-public ibus
  (package
   (name "ibus")
   (version "1.5.19")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/ibus/ibus/"
                                 "releases/download/"
                                 version "/ibus-" version ".tar.gz"))
             (sha256
              (base32
               "0a94bnpm24581317hdnihwr4cniriml10p4ffgxg14xhvaccfrjb"))))
   (build-system glib-or-gtk-build-system)
   (arguments
    `(#:tests? #f  ; tests fail because there's no connection to dbus
      #:configure-flags `("--disable-emoji-dict" ; cannot find emoji.json path
                          "--disable-python2"
                          "--enable-python-library"
                          ,(string-append "--with-ucd-dir="
                                          (getcwd) "/ucd")
                          "--enable-wayland")
      #:make-flags
      (list "CC=gcc"
            (string-append "pyoverridesdir="
                           (assoc-ref %outputs "out")
                           "/lib/python3.6/site-packages/gi/overrides/"))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'prepare-ucd-dir
          (lambda* (#:key inputs #:allow-other-keys)
            (mkdir-p "../ucd")
            (symlink (assoc-ref inputs "unicode-blocks") "../ucd/Blocks.txt")
            (symlink (assoc-ref inputs "unicode-nameslist") "../ucd/NamesList.txt")
            #t))
        (add-before 'configure 'disable-dconf-update
          (lambda _
            (substitute* "data/dconf/Makefile.in"
              (("dconf update") "echo dconf update"))
            #t))
        (add-after 'unpack 'delete-generated-files
          (lambda _
            (for-each (lambda (file)
                        (let ((c (string-append (string-drop-right file 4) "c")))
                          (when (file-exists? c)
                            (format #t "deleting ~a\n" c)
                            (delete-file c))))
                      (find-files "." "\\.vala"))
            #t))
        (add-after 'unpack 'fix-paths
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "src/ibusenginesimple.c"
              (("/usr/share/X11/locale")
               (string-append (assoc-ref inputs "libx11")
                              "/share/X11/locale")))
            (substitute* "ui/gtk3/xkblayout.vala"
              (("\"(setxkbmap|xmodmap)\"" _ prog)
               (string-append "\"" (assoc-ref inputs prog) "\"")))
            #t))
        (add-after 'wrap-program 'wrap-with-additional-paths
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Make sure 'ibus-setup' runs with the correct PYTHONPATH and
            ;; GI_TYPELIB_PATH.
            (let ((out (assoc-ref outputs "out")))
              (wrap-program (string-append out "/bin/ibus-setup")
                `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))
                `("GI_TYPELIB_PATH" ":" prefix
                  (,(getenv "GI_TYPELIB_PATH")
                   ,(string-append out "/lib/girepository-1.0")))))
            #t)))))
   (inputs
    `(("dbus" ,dbus)
      ("dconf" ,dconf)
      ("gconf" ,gconf)
      ("gtk2" ,gtk+-2)
      ("gtk+" ,gtk+)
      ("intltool" ,intltool)
      ("json-glib" ,json-glib)
      ("libnotify" ,libnotify)
      ("libx11" ,libx11)
      ("setxkbmap" ,setxkbmap)
      ("wayland" ,wayland)
      ("xmodmap" ,xmodmap)
      ("iso-codes" ,iso-codes)
      ("pygobject2" ,python-pygobject)
      ("python" ,python)))
   (native-inputs
    `(("glib" ,glib "bin") ; for glib-genmarshal
      ("gobject-introspection" ,gobject-introspection) ; for g-ir-compiler
      ("unicode-nameslist"
       ,(origin
          (method url-fetch)
          (uri "https://www.unicode.org/Public/UNIDATA/NamesList.txt")
          (sha256
           (base32 "0yr2h0nfqhirfi3bxl33z6cc94qqshlpgi06c25xh9754irqsgv8"))))
      ("unicode-blocks"
       ,(origin
          (method url-fetch)
          (uri "https://www.unicode.org/Public/UNIDATA/Blocks.txt")
          (sha256
           (base32 "0lnh9iazikpr548bd7nkaq9r3vfljfvz0rg2462prac8qxk7ni8b"))))
      ("vala" ,vala)
      ("pkg-config" ,pkg-config)))
   (native-search-paths
    (list (search-path-specification
           (variable "IBUS_COMPONENT_PATH")
           (files '("share/ibus/component")))))
   (synopsis "Input method framework")
   (description
    "IBus is an input framework providing a full-featured and user-friendly
input method user interface.  It comes with multilingual input support.  It
may also simplify input method development.")
   (home-page "https://github.com/ibus/ibus/wiki")
   (license lgpl2.1+)))

(define-public ibus-libpinyin
  (package
   (name "ibus-libpinyin")
   (version "1.10.0")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/libpinyin/ibus-libpinyin/"
                                 "releases/download/" version
                                 "/ibus-libpinyin-" version ".tar.gz"))
             (sha256
              (base32
               "0yq8aw4lddiviag8cnik6fp52vvk8lxv6bym13a3xya84c6zii3c"))))
   (build-system glib-or-gtk-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'wrap-program 'wrap-with-additional-paths
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; Make sure 'ibus-setup-libpinyin' runs with the correct
            ;; PYTHONPATH and GI_TYPELIB_PATH.
            (let ((out (assoc-ref outputs "out")))
              (wrap-program (string-append out "/libexec/ibus-setup-libpinyin")
                `("PYTHONPATH" ":" prefix
                  (,(getenv "PYTHONPATH")
                   ,(string-append (assoc-ref inputs "ibus")
                                   "/lib/girepository-1.0")))
                `("GI_TYPELIB_PATH" ":" prefix
                  (,(string-append (assoc-ref inputs "ibus")
                                   "/lib/girepository-1.0"))))
              #t))))))
   (inputs
    `(("ibus" ,ibus)
      ("libpinyin" ,libpinyin)
      ("bdb" ,bdb)
      ("sqlite" ,sqlite)
      ("python" ,python)
      ("pyxdg" ,python-pyxdg)
      ("gtk+" ,gtk+)))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("intltool" ,intltool)
      ("glib" ,glib "bin")))
   (synopsis "Chinese pinyin and ZhuYin input methods for IBus")
   (description
    "This package includes a Chinese pinyin input method and a Chinese
ZhuYin (Bopomofo) input method based on libpinyin for IBus.")
   (home-page "https://github.com/libpinyin/ibus-libpinyin")
   (license gpl2+)))

(define-public libpinyin
  (package
    (name "libpinyin")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libpinyin/libpinyin/"
                                  "releases/download/" version
                                  "/libpinyin-2.2.0.tar.gz"))
              (sha256
               (base32
                "1c4wxvcvjxvk23mcwqvsfsv4nhimx4kpjhabxa28gx1ih10l88gj"))))
    (build-system gnu-build-system)
    (inputs
     `(("glib" ,glib)
       ("bdb" ,bdb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Library to handle Chinese pinyin")
    (description
     "The libpinyin C++ library provides algorithms needed for sentence-based
Chinese pinyin input methods.")
    (home-page "https://github.com/libpinyin/libpinyin")
    (license gpl2+)))

(define-public ibus-anthy
  (package
    (name "ibus-anthy")
    (version "1.5.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ibus/ibus-anthy/releases/download/"
                    version "/ibus-anthy-" version ".tar.gz"))
              (sha256
               (base32
                "1y8sf837rmp662bv6zakny0xcm7c9c5qda7f9kq9riv9ywpcbw6x"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; Use absolute exec path in the anthy.xml.
       (list (string-append "--libexecdir=" %output "/libexec"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (prog)
                  (wrap-program (string-append out "/libexec/" prog)
                    `("PYTHONPATH" ":" prefix
                      (,(getenv "PYTHONPATH")))
                    `("GI_TYPELIB_PATH" ":" prefix
                      (,(getenv "GI_TYPELIB_PATH")
                       ,(string-append out "/lib/girepository-1.0")))))
                '("ibus-engine-anthy" "ibus-setup-anthy"))
               #t))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
    (inputs
     `(("anthy" ,anthy)
       ("gtk+" ,gtk+)
       ("ibus" ,ibus)
       ("gobject-introspection" ,gobject-introspection)
       ("python-pygobject" ,python-pygobject)))
    (synopsis "Anthy Japanese language input method for IBus")
    (description "IBus-Anthy is an engine for the input bus \"IBus\").  It
adds the Anthy Japanese language input method to IBus.  Because most graphical
applications allow text input via IBus, installing this package will enable
Japanese language input in most graphical applications.")
    (home-page "https://github.com/fujiwarat/ibus-anthy")
    (license gpl2+)))
