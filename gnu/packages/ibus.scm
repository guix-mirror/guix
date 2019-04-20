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
    (version "1.5.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ibus/ibus/"
                                  "releases/download/"
                                  version "/ibus-" version ".tar.gz"))
              (sha256
               (base32
                "0d6hcbw6ai91jl87lqnyn8bxi5y5kba5i9nz7knknyh69g5fbwac"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f  ; tests fail because there's no connection to dbus
       #:configure-flags `("--disable-emoji-dict" ; cannot find emoji.json path
                           "--enable-python-library"
                           ,(string-append "--with-ucd-dir="
                                           (getcwd) "/ucd")
                           "--enable-wayland")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-ucd-dir
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "../ucd")
             (symlink (assoc-ref inputs "unicode-blocks") "../ucd/Blocks.txt")
             (symlink (assoc-ref inputs "unicode-nameslist") "../ucd/NamesList.txt")
             #t))
         (add-after 'unpack 'patch-python-target-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((root (string-append (assoc-ref outputs "out")
                                        "/lib/python"
                                        ,(version-major+minor (package-version python))
                                        "/site-packages")))
               (substitute* "configure"
                 (("(py2?overridesdir)=.*" _ var)
                  (string-append var "=" root "/gi/overrides/"))
                 (("(pkgpython2dir=).*" _ var)
                  (string-append var root "/ibus"))))
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
       ("gettext" ,gnu-gettext)
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

       ;; XXX TODO: Move Unicode data to its own (versioned) package.
       ("unicode-nameslist"
        ,(origin
           (method url-fetch)
           (uri "https://www.unicode.org/Public/12.0.0/ucd/NamesList.txt")
           (sha256
            (base32 "0vsq8gx7hws8mvxy3nlglpwxw7ky57q0fs09d7w9xgb2ylk7fz61"))))
       ("unicode-blocks"
        ,(origin
           (method url-fetch)
           (uri "https://www.unicode.org/Public/12.0.0/ucd/Blocks.txt")
           (sha256
            (base32 "041sk54v6rjzb23b9x7yjdwzdp2wc7gvfz7ybavgg4gbh51wm8x1"))))
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
    (version "0.38.20190131")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/plum.git")
             (commit "8b48688cd4610d0c9223eb68831a31b6134e4cc8")))
       (file-name "plum-checkout")
       (sha256
        (base32 "0fv3hv4av9y7afxijh1n8idnyf82v9rxxi2ypmxd7lkj4naa22qh"))))
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
                           (symlink (assoc-ref inputs
                                               (string-append "rime-" pkg))
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
     `(("rime-array"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-array.git")
                 (commit "93cc99238f120606a076220ec4ddcad164f6396a")))
           (file-name "rime-array-checkout")
           (sha256
            (base32
             "06yxrv3x702751jvx46rfw3ix34jk7jh183rz6bllznzi7lxz7sz"))))
       ("rime-bopomofo"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-bopomofo.git")
                 (commit "ed25098386f5efd3d70b08650f0d1b70c41c11a3")))
           (file-name "rime-bopomofo-checkout")
           (sha256
            (base32
             "1ip1pbfb1hadf2mcymr5939iagf25ywfl67d9198jahzyr6rdyvc"))))
       ("rime-cangjie"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-cangjie.git")
                 (commit "5fd8ce6f64039e505ca02655a621f2e830b97c19")))
           (file-name "rime-cangjie-checkout")
           (sha256
            (base32
             "1gf6r0q593ixar6v0jyvs56cik2gjp7pf9v799rfd2yydyia3bfg"))))
       ("rime-combo-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-combo-pinyin.git")
                 (commit "9bd952b964e9744e5d18e9e31625b50f3585a2cb")))
           (file-name "rime-combo-pinyin-checkout")
           (sha256
            (base32
             "0crafjs39x4j221gb34mxxh3cdpxfhhx3nfw6b6bgkzlrp35a02b"))))
       ("rime-double-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-double-pinyin.git")
                 (commit "69bf85d4dfe8bac139c36abbd68d530b8b6622ea")))
           (file-name "rime-double-pinyin-checkout")
           (sha256
            (base32
             "093wif5avvvw45fqbwj5wkbxrychy4pagl4mwsmbrayc8jkp69ak"))))
       ("rime-emoji"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-emoji.git")
                 (commit "c8d67f9b50bf89a10c57da646d2e6db8799aef38")))
           (file-name "rime-emoji-checkout")
           (sha256
            (base32
             "0ngcm088iyyp3llxvv0m80i7n5928d6cgh256ikhn3ixallxxdxv"))))
       ("rime-essay"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-essay.git")
                 (commit "71d0b1f3d4f3bfe61ae07581edf07579740c4421")))
           (file-name "rime-essay-checkout")
           (sha256
            (base32
             "1iwz104k7zfk7lpa257kvpqdr6jhbg3p76n3644ywiz4l7kc678i"))))
       ("rime-ipa"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-ipa.git")
                 (commit "e420c7bfb07153a2d2484eb2bdccdd719811abbb")))
           (file-name "rime-ipa-checkout")
           (sha256
            (base32
             "1wfv7lb4y61b3qic5mhw92rn46cckldd9wrkcq03mg5512mbw63z"))))
       ("rime-jyutping"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-jyutping.git")
                 (commit "04891a298319888e8c6b1a20e0fa81cfaea01264")))
           (file-name "rime-jyutping-checkout")
           (sha256
            (base32
             "0wsj965khglz36cnvfm4fkv386xvxhmsxgcw88p5qi0b3wlbzzx6"))))
       ("rime-luna-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-luna-pinyin.git")
                 (commit "c9c405566177cb3898bdb82d7f4157587f2d9c01")))
           (file-name "rime-luna-pinyin-checkout")
           (sha256
            (base32
             "0i7f2675lvj9pzwlm8550ifnr3xqi77xlyyvml1wpxpkfqhjr475"))))
       ("rime-middle-chinese"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-middle-chinese.git")
                 (commit "ed6d44f8d0bedf6e0c1c3183a270f8f01a211a40")))
           (file-name "rime-middle-chinese-checkout")
           (sha256
            (base32
             "09mql88lsrxa99pyllby5z22kaiwwa037ha8gwaxjnnlsjgvz7zx"))))
       ("rime-pinyin-simp"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-pinyin-simp.git")
                 (commit "bb5a6dfa871669d2f451b477bfff6d131df6f6c6")))
           (file-name "rime-pinyin-simp-checkout")
           (sha256
            (base32
             "0ss82042k833w5q72h72ghcfchkx00nx6l4z4fb861s2rxr0bkjd"))))
       ("rime-prelude"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-prelude.git")
                 (commit "8a52b4f86a59f3eb602f9a4cf6a680a67c15df8c")))
           (file-name "rime-prelude-checkout")
           (sha256
            (base32
             "039fr3996vfxzn2milaq1f5fw08f6zgjsxsql6cfhsc5b55fidm7"))))
       ("rime-quick"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-quick.git")
                 (commit "3fe5911ba608cb2df1b6301b76ad1573bd482a76")))
           (file-name "rime-quick-checkout")
           (sha256
            (base32
             "08bh87ym5qvw55lyw20l3m7jd4c2z5rvil8h5q8790r7z6j6ijy9"))))
       ("rime-scj"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-scj.git")
                 (commit "cab5a0858765eff0553dd685a2d61d5536e9149c")))
           (file-name "rime-scj-checkout")
           (sha256
            (base32
             "0ard2bjp4896a8dimmcwyjwgmp9kl4rz92yc92jnd3y4rgwl6fvk"))))
       ("rime-soutzoe"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-soutzoe.git")
                 (commit "beeaeca72d8e17dfd1e9af58680439e9012987dc")))
           (file-name "rime-soutzoe-checkout")
           (sha256
            (base32
             "0jyqx0q9s0qxn168l5n8zav8jcl2g5ppr7pa8jm1vwrllf20slcc"))))
       ("rime-stenotype"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-stenotype.git")
                 (commit "1d472097c32d943d1096644f4c31f28799a17bd8")))
           (file-name "rime-stenotype-checkout")
           (sha256
            (base32
             "1dy9qlbyhnshq2k1vcvkqn2624r96iaixhyrx1z7v0vz84fjf6y4"))))
       ("rime-stroke"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-stroke.git")
                 (commit "f802735392b378fb2a56a9b7b53b8ec96a30ccaf")))
           (file-name "rime-stroke-checkout")
           (sha256
            (base32
             "1wlrsskxhldh8369n771gk7sxflzdx0c9qhq1mqm5hhkwc5ig1j0"))))
       ("rime-terra-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-terra-pinyin.git")
                 (commit "b9e0edd3182e74b26b03a278c038e669ca538a35")))
           (file-name "rime-terra-pinyin-checkout")
           (sha256
            (base32
             "1vzrv2k178bii4ld9rvpdi8zmcwybd8bks0qzjx2v4kbjgwj28zk"))))
       ("rime-wubi"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-wubi.git")
                 (commit "dd052ee158a38cb791755318b1aef9b4a3ed0316")))
           (file-name "rime-wubi-checkout")
           (sha256
            (base32
             "00xzv3sbwqh2jz4i7s315h7rw17qa2dgj7kflyy3blxk0s2cqiqa"))))
       ("rime-wugniu"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-wugniu.git")
                 (commit "abd1ee98efbf170258fcf43875c21a4259e00b61")))
           (file-name "rime-wugniu-checkout")
           (sha256
            (base32
             "0qn54d3cclny106ixdw08r5n6wn52ffs1hgrma3k0j4pv0kr9nlq"))))))
    (home-page "https://rime.im/")
    (synopsis "Schema data of Rime Input Method Engine")
    (description "@dfn{rime-data} provides the schema data of Rime Input
Method Engine.")
    (license lgpl3+)))

(define-public ibus-rime
  (package
    (name "ibus-rime")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/ibus-rime.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12y6jdz1amhgrnqa7zjim63dfsz6zyxyahbirfan37wmcfp6gp1d"))))
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
