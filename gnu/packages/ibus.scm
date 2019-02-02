;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Meiyo Peng <meiyo@disroot.org>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages anthy)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages textutils)
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
                            "/lib/python"
                            ,(version-major+minor (package-version python))
                            "/site-packages/gi/overrides/"))
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

(define-public librime
  (package
    (name "librime")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/librime.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zkx1wfbd94v55gfycyd2b94jxclfyk2zl7yw35pyjx63qdlb6sd"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "thirdparty/src")
           (delete-file-recursively "thirdparty/bin")
           (delete-file-recursively "thirdparty/include/X11")
           #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "CMakeLists.txt"
               (("include_directories\\($\\{PROJECT_SOURCE_DIR\\}/thirdparty/include\\)") "")
               (("link_directories\\($\\{PROJECT_SOURCE_DIR\\}/thirdparty/lib\\)") ""))
             #t)))))
    (inputs
     `(("boost" ,boost)
       ("glog" ,glog)
       ("leveldb" ,leveldb)
       ("marisa" ,marisa)
       ("opencc" ,opencc)
       ("yaml-cpp" ,yaml-cpp)))
    (native-inputs
     `(("googletest" ,googletest)
       ("xorgproto" ,xorgproto))) ; keysym.h
    (home-page "https://rime.im/")
    (synopsis "The core library of Rime Input Method Engine")
    (description "@dfn{librime} is the core library of Rime Input Method
Engine, which is a lightweight, extensible input method engine supporting
various input schemas including glyph-based input methods, romanization-based
input methods as well as those for Chinese dialects.  It has the ability to
compose phrases and sentences intelligently and provide very accurate
traditional Chinese output.")
    (license bsd-3)))

(define-public rime-data
  (package
    (name "rime-data")
    (version "0.38.20181029")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/plum.git")
             (commit "fb4f829da2007f2dbb37d60a79bc67c25ea16568")))
       (file-name "plum-checkout")
       (sha256
        (base32 "1m1wiv9j5bay4saga58c7dj4h8gqivsbyp16y245ifvxvp9czj67"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                  ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "no_update=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; Set .DEFAULT_GOAL to `all'.
             ;; Don't build binary schemas. The output is not deterministic.
             (substitute* "Makefile"
               (("^\\.DEFAULT_GOAL := preset")
                ".DEFAULT_GOAL := all"))
             #t))
         ;; Add schema packages into "package/rime" directory.
         (add-after 'unpack 'add-packages
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((dest-dir "package/rime"))
               (mkdir-p dest-dir)
               (for-each (lambda (pkg)
                           (symlink (assoc-ref inputs pkg)
                                    (string-append dest-dir "/" pkg)))
                         '("array"
                           "bopomofo"
                           "cangjie"
                           "combo-pinyin"
                           "double-pinyin"
                           "emoji"
                           "essay"
                           "ipa"
                           "jyutping"
                           "luna-pinyin"
                           "middle-chinese"
                           "pinyin-simp"
                           "prelude"
                           "quick"
                           "scj"
                           "soutzoe"
                           "stenotype"
                           "stroke"
                           "terra-pinyin"
                           "wubi"
                           "wugniu")))
             #t))
         (delete 'configure))))
    (native-inputs
     `(("array"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-array.git")
                 (commit "906e923902147584b0b0247028a782abbfbfd8a0")))
           (file-name "rime-array-checkout")
           (sha256
            (base32
             "1alk6ghn4ji4kvp7lfm57bwm2gjh99i79r0w9naz6wkdim8idvb1"))))
       ("bopomofo"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-bopomofo.git")
                 (commit "8dc44ca1b6ef4e45b452e070b9da737f5da165e3")))
           (file-name "rime-bopomofo-checkout")
           (sha256
            (base32
             "16k6wfhcrw3a77rmbrp21ca0gmsmb3f68s193c1cfwr8i68k46nf"))))
       ("cangjie"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-cangjie.git")
                 (commit "ab085e90856b3399b374dc3c8b4cb40d11f307a8")))
           (file-name "rime-cangjie-checkout")
           (sha256
            (base32
             "11fgj0rbv9nyzfijwm2l8pm8fznhif4h27ndrrcaaylkp7p5zsx2"))))
       ("combo-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-combo-pinyin.git")
                 (commit "f1bae63f20504f2b8113c5cbdf2700e858aa91eb")))
           (file-name "rime-combo-pinyin-checkout")
           (sha256
            (base32
             "1l1079akwm1hw4kkn0q6x9fpylnl2ka6z2fn7lmdpfpsr0xgn0n7"))))
       ("double-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-double-pinyin.git")
                 (commit "2101a5cd40e511ec38835769aa66d2dddf059c2e")))
           (file-name "rime-double-pinyin-checkout")
           (sha256
            (base32
             "19hh2qm0njbfk2js678hfm2hw9b796s43vs11yy3m1v9m0gk2vi7"))))
       ("emoji"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-emoji.git")
                 (commit "6e6611b315f03ee4c33f958f9dbe960b13a0ed19")))
           (file-name "rime-emoji-checkout")
           (sha256
            (base32
             "1brfs3214w36j3345di9ygp468hbvbqdqpkjxxs1dbp437rayhyy"))))
       ("essay"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-essay.git")
                 (commit "5e5c7a0ef41c9b030abdad81a9df07b56b1661e9")))
           (file-name "rime-essay-checkout")
           (sha256
            (base32
             "0ana9is0zhh79m4gjshvmaxbrg3jiqysydx5bpm151i7i6vw5y1i"))))
       ("ipa"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-ipa.git")
                 (commit "02a9e2c181921a2e95e1a81f88188c41132755c3")))
           (file-name "rime-ipa-checkout")
           (sha256
            (base32
             "1szrxgvqlgmxapj2aflw2cvbv0p6pl0sw0gyxa13dvdhhf7s9rvr"))))
       ("jyutping"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-jyutping.git")
                 (commit "1402ec3d6cc0973f952fe3f9ef531294e4ffe9e0")))
           (file-name "rime-jyutping-checkout")
           (sha256
            (base32
             "17g03dy4gw6vyc9da1wjn3iy9hx64dfnwiwsfc7bkzan22x2m4dv"))))
       ("luna-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-luna-pinyin.git")
                 (commit "3b05132576f5c347ff8a70857d2dae080936ac3b")))
           (file-name "rime-luna-pinyin-checkout")
           (sha256
            (base32
             "0kgnpxjn10dm2d9718r12rdjlwqd2s2h84jvkhxhh5v0dkv1anl2"))))
       ("middle-chinese"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-middle-chinese.git")
                 (commit "9ba8d70330654b9a730f882d35cfad7dbeddfd75")))
           (file-name "rime-middle-chinese-checkout")
           (sha256
            (base32
             "0hwg5zby5kphh0bcfay8mfxwr5bwqhamiw3cmmmf7kp9fbns5s23"))))
       ("pinyin-simp"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-pinyin-simp.git")
                 (commit "74357ffd62c05fb60edf6eab5b86bc8c8c1907d0")))
           (file-name "rime-pinyin-simp-checkout")
           (sha256
            (base32
             "1paw3c7pv5bl54abnp9pidfxrkchdacyxy5m9zb311p5sgm7fhxh"))))
       ("prelude"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-prelude.git")
                 (commit "33040568c3ddb2ee6340c9b669494317db21b77c")))
           (file-name "rime-prelude-checkout")
           (sha256
            (base32
             "1gwcasyyg6f0ib6s4qsrrjcqr1lcs7j3xqxl65rznsw44nhnbwwq"))))
       ("quick"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-quick.git")
                 (commit "910a97d403ad8e72f322488da146da79c19d623f")))
           (file-name "rime-quick-checkout")
           (sha256
            (base32
             "0yrq3gbfmm29xlr52rmxc41mqfrb0295q7sdhbc3ax71677mpr0y"))))
       ("scj"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-scj.git")
                 (commit "e0eae889f4376d2a434ac3b38523e0da7400db68")))
           (file-name "rime-scj-checkout")
           (sha256
            (base32
             "1whnv9zs349kvy0zi7dnmpqwil8i6gqwrzvhy3qdrjzy58y6gwxn"))))
       ("soutzoe"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-soutzoe.git")
                 (commit "e47841a8ad6341731c41cdb814b7a25c837603c4")))
           (file-name "rime-soutzoe-checkout")
           (sha256
            (base32
             "1rgpmkxa72jy6gyy44fn8azpk3amk9s9lrdf7za03nv95d0fvm0p"))))
       ("stenotype"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-stenotype.git")
                 (commit "d4ff379314fd95283853d1734854979cf3cbd287")))
           (file-name "rime-stenotype-checkout")
           (sha256
            (base32
             "1kckpi4l4884hvydr3d6vid3v7rsc1app29kmk7v8jf8vn16afhl"))))
       ("stroke"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-stroke.git")
                 (commit "cfd29c675c46cf70b7a7f0a3836a913059316a0a")))
           (file-name "rime-stroke-checkout")
           (sha256
            (base32
             "135is9c1p4lm98fd9l1gxyflkm69cv5an129ka7sk614bq84m08d"))))
       ("terra-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-terra-pinyin.git")
                 (commit "15b5c73a796571cd6f9ef6c89f96656cb9df86f9")))
           (file-name "rime-terra-pinyin-checkout")
           (sha256
            (base32
             "1xsd84h1zw417h5hr4dbgyk5009zi7q2p9774w3ccr5sxgc3i3cm"))))
       ("wubi"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-wubi.git")
                 (commit "d44403728a0b1cd8b47cb1f81b83f58e5f790b74")))
           (file-name "rime-wubi-checkout")
           (sha256
            (base32
             "0ld31bdn94lncxd1ka44w4sbl03skh08mc927dhdmwq5bpvrgn36"))))
       ("wugniu"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-wugniu.git")
                 (commit "65bcc354ada3839591d7546a64c71dbdd0592b02")))
           (file-name "rime-wugniu-checkout")
           (sha256
            (base32
             "0g31awp40s778sp5c290x40s8np86n8aw011s17sslxrqhhb0bkx"))))))
    (home-page "https://rime.im/")
    (synopsis "Schema data of Rime Input Method Engine")
    (description "@dfn{rime-data} provides the schema data of Rime Input
Method Engine.")
    (license lgpl3+)))

(define-public ibus-rime
  (package
    (name "ibus-rime")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/ibus-rime.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nqi7ymv34a9kx24say3xj98lkrs9nkpv1n2ijb91wdz3cr012ly"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Define RIME_DATA_DIR. It's required but not used by the code.
             (substitute* "Makefile"
               (("cmake")
                (string-append "cmake -DRIME_DATA_DIR="
                               (assoc-ref inputs "rime-data")
                               "/share/rime-data")))
             ;; rime_config.h defines the actual data directory.
             (substitute* "rime_config.h"
               (("^#define IBUS_RIME_INSTALL_PREFIX .*$")
                (string-append "#define IBUS_RIME_INSTALL_PREFIX \""
                               (assoc-ref outputs "out")
                               "\"\n"))
               (("^#define IBUS_RIME_SHARED_DATA_DIR .*$")
                (string-append "#define IBUS_RIME_SHARED_DATA_DIR \""
                               (assoc-ref inputs "rime-data")
                               "/share/rime-data\"\n")))
             #t))
         (delete 'configure))))
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("ibus" ,ibus)
       ("libnotify" ,libnotify)
       ("librime" ,librime)
       ("rime-data" ,rime-data)))
    (native-inputs
     `(("cmake" ,cmake)
       ("pkg-config" ,pkg-config)))
    (home-page "https://rime.im/")
    (synopsis "Rime Input Method Engine for IBus")
    (description "@dfn{ibus-rime} provides the Rime input method engine for
IBus.  Rime is a lightweight, extensible input method engine supporting
various input schemas including glyph-based input methods, romanization-based
input methods as well as those for Chinese dialects.  It has the ability to
compose phrases and sentences intelligently and provide very accurate
traditional Chinese output.")
    (license gpl3+)))
