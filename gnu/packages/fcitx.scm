;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
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

(define-module (gnu packages fcitx)
  #:use-module ((guix licenses) #:select (gpl2+ bsd-3))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages check)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg))

(define-public fcitx-qt5
  (package
    (name "fcitx-qt5")
    (version "1.2.6")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/fcitx/fcitx-qt5.git")
         (commit version)))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "13sanrir696fv7s44b7q453s5qi4r7ag0r3iyggyzk8xyf6rw8fk"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-install-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "quickphrase-editor/CMakeLists.txt"
               (("\\$\\{FCITX4_ADDON_INSTALL_DIR\\}")
                (string-append
                 (assoc-ref outputs "out")
                 "/lib/fcitx")))
             (substitute* "platforminputcontext/CMakeLists.txt"
               (("\\$\\{CMAKE_INSTALL_QTPLUGINDIR\\}")
                (string-append
                 (assoc-ref outputs "out")
                 "/lib/qt5/plugins")))
             #t)))))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     `(("fcitx" ,fcitx)
       ("libintl" ,intltool)
       ("libxkbcommon" ,libxkbcommon)))
    (propagated-inputs
     (list qtbase-5))
    (synopsis "Fcitx Qt5 Input Context")
    (description "This package provides a Qt5 frontend for fcitx.")
    (home-page "https://github.com/fcitx/fcitx-qt5/")
    (license
     (list
      ;; Plugin
      bsd-3
      ;; Others
      gpl2+))))

(define-public presage
  (package
    (name "presage")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/presage/presage/"
                       version "/presage-" version ".tar.gz"))
       (sha256
        (base32 "0rm3b3zaf6bd7hia0lr1wyvi1rrvxkn7hg05r5r1saj0a3ingmay"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "CFLAGS=-Wno-narrowing"
        "CXXFLAGS=-Wno-narrowing")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share/presage"))
               (rename-file
                (string-append out "/share/presage/html")
                (string-append doc "/share/presage/html"))
               #t))))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("help2man" ,help2man)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)))
    (inputs
     (list glib gtk+ libx11 sqlite tinyxml))
    (synopsis "Intelligent Predictive Text Entry System")
    (description "Presage generates predictions by modelling natural language as
a combination of redundant information sources.  It computes probabilities for
words which are most likely to be entered next by merging predictions generated
by the different predictive algorithms.")
    (home-page "https://presage.sourceforge.io/")
    (license gpl2+)))

(define-public fcitx
  (package
    (name "fcitx")
    (version "4.2.9.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.fcitx-im.org/fcitx/"
                                  "fcitx-" version "_dict.tar.xz"))
              (sha256
               (base32
                "1iik80l7g8yk9iwsls6nl9whwgm0sj8i7s6s0bz4c5anl35iaddw"))))
    (build-system cmake-build-system)
    (outputs '("out" "gtk2" "gtk3"))
    (arguments
     `(#:configure-flags
       (list "-DENABLE_TEST=ON"
             (string-append "-DXKB_RULES_XML_FILE="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb/rules/evdev.xml")
             "-DENABLE_GTK2_IM_MODULE=ON"
             "-DENABLE_GTK3_IM_MODULE=ON"
             (string-append "-DGTK2_IM_MODULEDIR="
                            (assoc-ref %outputs "gtk2")
                            "/lib/gtk-2.0/2.10.0/immodules")
             (string-append "-DGTK3_IM_MODULEDIR="
                            (assoc-ref %outputs "gtk3")
                            "/lib/gtk-3.0/3.0.0/immodules")
             ;; XXX: Enable GObject Introspection and Qt4 support.
             "-DENABLE_GIR=OFF"
             "-DENABLE_QT=OFF"
             "-DENABLE_QT_IM_MODULE=OFF")))
    (native-inputs
     `(("doxygen"    ,doxygen)
       ("extra-cmake-modules"
        ;; XXX: We can't simply #:use-module due to a cycle somewhere.
        ,(module-ref
          (resolve-interface '(gnu packages kde-frameworks))
          'extra-cmake-modules))
       ("glib:bin"   ,glib "bin")       ; for glib-genmarshal
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("dbus"             ,dbus)
       ("enchant"          ,enchant-1.6)
       ("gettext"          ,gettext-minimal)
       ("gtk2"             ,gtk+-2)
       ("gtk3"             ,gtk+)
       ("icu4c"            ,icu4c)
       ("iso-codes"        ,iso-codes)
       ("json-c"           ,json-c)
       ("libxkbfile"       ,libxkbfile)
       ("libxml2"          ,libxml2)
       ("xkeyboard-config" ,xkeyboard-config)))
    (home-page "https://fcitx-im.org")
    (synopsis "Input method framework")
    (description
     "Fcitx is an input method framework with extension support.  It has
Pinyin, Quwei and some table-based (Wubi, Cangjie, Erbi, etc.) input methods
built-in.")
    (license gpl2+)))

(define-public fcitx-configtool
  (package
   (name "fcitx-configtool")
   (version "0.4.10")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://download.fcitx-im.org/fcitx-configtool/"
                                "fcitx-configtool-" version ".tar.xz"))
            (sha256
             (base32
              "1yyi9jhkwn49lx9a47k1zbvwgazv4y4z72gnqgzdpgdzfrlrgi5w"))))
   (build-system cmake-build-system)
   (arguments
    `(#:configure-flags
      (list "-DENABLE_GTK2=ON"
            "-DENABLE_GTK3=ON")
      #:tests? #f)) ; No tests.
   (native-inputs
    `(("glib:bin"   ,glib "bin")
      ("pkg-config" ,pkg-config)))
   (inputs
    `(("fcitx"      ,fcitx)
      ("dbus-glib"  ,dbus-glib)
      ("gettext"    ,gettext-minimal)
      ("gtk2"       ,gtk+-2)
      ("gtk3"       ,gtk+)
      ("iso-codes"  ,iso-codes)))
   (home-page "https://fcitx-im.org/wiki/Configtool")
   (synopsis "Graphic Fcitx configuration tool")
   (description
    "Fcitx is an input method framework with extension support.  It has
Pinyin, Quwei and some table-based (Wubi, Cangjie, Erbi, etc.) input methods
built-in.  This package provides GTK version of the graphic configuration
tool of Fcitx.")
   (license gpl2+)))
