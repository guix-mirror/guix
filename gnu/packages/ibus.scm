;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020 Peng Mei Yu <i@pengmeiyu.com>
;;; Copyright © 2020 kanichos <kanichos@yandex.ru>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
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
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages xorg))

(define-public ibus
  (package
    (name "ibus")
    (version "1.5.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ibus/ibus/"
                                  "releases/download/"
                                  version "/ibus-" version ".tar.gz"))
              (sha256
               (base32
                "0jmy2w01phpmqnjnfnak7nvfna57mpgfnl87jwc4iai8ijjynw41"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f  ; tests fail because there's no connection to dbus
       #:parallel-build? #f ; race condition discovered with emoji support
       #:configure-flags (list "--enable-python-library"
                               (string-append
                                "--with-unicode-emoji-dir="
                                (assoc-ref %build-inputs "unicode-emoji")
                                "/share/unicode/emoji")
                               (string-append
                                "--with-emoji-annotation-dir="
                                (assoc-ref %build-inputs "unicode-cldr-common")
                                "/share/unicode/cldr/common/annotations")
                               (string-append "--with-ucd-dir="
                                              (assoc-ref %build-inputs "ucd")
                                              "/share/ucd")
                               "--enable-wayland")
       #:phases
       (modify-phases %standard-phases
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
                (string-append "\"" (assoc-ref inputs prog) "/bin/" prog "\"")))
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
       ("gettext" ,gettext-minimal)
       ("gobject-introspection" ,gobject-introspection) ; for g-ir-compiler
       ("ucd" ,ucd)
       ("unicode-emoji" ,unicode-emoji)
       ("unicode-cldr-common" ,unicode-cldr-common)
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libpinyin/ibus-libpinyin/"
                                  "releases/download/" version
                                  "/ibus-libpinyin-" version ".tar.gz"))
              (sha256
               (base32
                "0xl2lmffy42f6h6za05z4vpazpza1a9gsrva65giwyv3kpf652dd"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       '("--enable-opencc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'wrap-program 'wrap-with-additional-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'ibus-setup-libpinyin' runs with the correct
             ;; PYTHONPATH and GI_TYPELIB_PATH.
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/libexec/ibus-setup-libpinyin")
                 `("PYTHONPATH" ":" =
                   (,(getenv "PYTHONPATH")
                    ,(string-append (assoc-ref inputs "ibus")
                                    "/lib/girepository-1.0")
                    ,(string-append (assoc-ref outputs "out")
                                    "/share/ibus-libpinyin/setup/")))
                 `("GI_TYPELIB_PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "ibus")
                                    "/lib/girepository-1.0")
                    ,(string-append (assoc-ref outputs "out")
                                    "/share/ibus-libpinyin/setup/"))))
               #t))))))
    (inputs
     `(("ibus" ,ibus)
       ("libpinyin" ,libpinyin)
       ("bdb" ,bdb)
       ("sqlite" ,sqlite)
       ("opencc" ,opencc)
       ("python" ,python)
       ("pygobject2" ,python-pygobject)
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
    (license gpl3+)))

(define-public libpinyin
  (package
    (name "libpinyin")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libpinyin/libpinyin/"
                                  "releases/download/" version
                                  "/libpinyin-" version ".tar.gz"))
              (sha256
               (base32
                "10h5mjgv4ibhispvr3s1k36a4aclx4dcvcc2knd4sg1xibw0dp4w"))))
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
    (version "1.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/librime")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1avmy2yyag22cl2j8085n5czsk93sxv440pdb3a2diwcxwwmzm9v"))
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
       ("capnproto" ,capnproto)
       ("glog" ,glog)
       ("leveldb" ,leveldb)
       ("marisa" ,marisa)
       ("opencc" ,opencc)
       ("yaml-cpp" ,yaml-cpp)))
    (native-inputs
     `(("googletest" ,googletest)
       ("pkg-config" ,pkg-config)
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
    (version "0.38.20200623")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/plum")
             (commit "397d601dd22cfc857613973724724b8f44db9f9c")))
       (file-name "plum-checkout")
       (sha256
        (base32 "06ad5c4m7xsfr4if5ywshfj2aj5g5b5hwzh38dzccn7c1l2ibi0z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((ice-9 match)
                  ,@%gnu-build-system-modules)
       #:tests? #f                  ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "no_update=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; Don't build binary Rime schema.  The binary Rime schema files
             ;; are platform dependent and contains timestamp information.
             ;; Thus they are not reproducible.

             ;; Change `.DEFAULT_GOAL' to `all'.
             (substitute* "Makefile"
               (("^\\.DEFAULT_GOAL := preset")
                ".DEFAULT_GOAL := all"))
             ;; Disable git operations.
             (substitute* "scripts/install-packages.sh"
               ((".*update-package\\.sh.*") ""))
             #t))
         ;; Copy Rime schemas into the "package/rime" directory.
         (add-after 'unpack 'copy-rime-schemas
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((dest-dir "package/rime"))
               (mkdir-p dest-dir)
               (for-each
                (match-lambda
                  ((name . path)
                   (if (string-prefix? "rime-" name)
                       (let ((schema (substring name (string-length "rime-"))))
                         (symlink path (string-append dest-dir "/" schema))))))
                inputs))
             #t))
         (delete 'configure))))
    (inputs
     `(("rime-array"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-array")
                 (commit "93cc99238f120606a076220ec4ddcad164f6396a")))
           (file-name "rime-array-checkout")
           (sha256
            (base32
             "06yxrv3x702751jvx46rfw3ix34jk7jh183rz6bllznzi7lxz7sz"))))
       ("rime-bopomofo"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-bopomofo")
                 (commit "ed25098386f5efd3d70b08650f0d1b70c41c11a3")))
           (file-name "rime-bopomofo-checkout")
           (sha256
            (base32
             "1ip1pbfb1hadf2mcymr5939iagf25ywfl67d9198jahzyr6rdyvc"))))
       ("rime-cangjie"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-cangjie")
                 (commit "5fd8ce6f64039e505ca02655a621f2e830b97c19")))
           (file-name "rime-cangjie-checkout")
           (sha256
            (base32
             "1gf6r0q593ixar6v0jyvs56cik2gjp7pf9v799rfd2yydyia3bfg"))))
       ("rime-cantonese"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-cantonese")
                 (commit "e06fe8e79d7d64db2f2b6339aabc004d8cbc1f67")))
           (file-name "rime-cantonese-checkout")
           (sha256
            (base32
             "0j6kbhdfj7dx812yzykndzbk53s2v1rsaa2jlyma03hz7qlnnl0s"))))
       ("rime-combo-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-combo-pinyin")
                 (commit "67b29cdc786928ea46b43a9c660dee3db8f1adff")))
           (file-name "rime-combo-pinyin-checkout")
           (sha256
            (base32
             "1v6ax51xll2aizbz1xzjyk6p3lmq8cyzbxkrwcffa723zaj0zz4l"))))
       ("rime-double-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-double-pinyin")
                 (commit "69bf85d4dfe8bac139c36abbd68d530b8b6622ea")))
           (file-name "rime-double-pinyin-checkout")
           (sha256
            (base32
             "093wif5avvvw45fqbwj5wkbxrychy4pagl4mwsmbrayc8jkp69ak"))))
       ("rime-emoji"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-emoji")
                 (commit "c99d34e4a837349e4679a110bb4b94f71fe015ae")))
           (file-name "rime-emoji-checkout")
           (sha256
            (base32
             "1wiwlxjjml9xfgg7z1wzaf4b1bsg81dkwvsfff2b61fwxq61zkgw"))))
       ("rime-essay"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-essay")
                 (commit "88055afa3752e4582fa887765d962a30e02bb1fa")))
           (file-name "rime-essay-checkout")
           (sha256
            (base32
             "0ap7xqv6v0x6mdkw2cv93cbr6qhpla3803z04522wb8l9hr7iryg"))))
       ("rime-ipa"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-ipa")
                 (commit "22b71710e029bcb412e9197192a638ab11bc2abf")))
           (file-name "rime-ipa-checkout")
           (sha256
            (base32
             "0zdk4f9qkfj3q5hmjnairj1lv6f6y27mic12k886n6sxywwbwr2k"))))
       ("rime-jyutping"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-jyutping")
                 (commit "6fe0d727b3178feabd0f01e6cd82599202764735")))
           (file-name "rime-jyutping-checkout")
           (sha256
            (base32
             "0wz6d3pmi72ysh2c0nml3rsz9hd2vazsyhnz34gq26yf4j85phfs"))))
       ("rime-luna-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-luna-pinyin")
                 (commit "f1268e192ca88b9526467ce04ac3e47c837891ad")))
           (file-name "rime-luna-pinyin-checkout")
           (sha256
            (base32
             "0nxnjp1ybcrsan1mxnzwbkfhwl99kza6i9k1s7m9wzmhv7x7zahg"))))
       ("rime-middle-chinese"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-middle-chinese")
                 (commit "ed6d44f8d0bedf6e0c1c3183a270f8f01a211a40")))
           (file-name "rime-middle-chinese-checkout")
           (sha256
            (base32
             "09mql88lsrxa99pyllby5z22kaiwwa037ha8gwaxjnnlsjgvz7zx"))))
       ("rime-pinyin-simp"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-pinyin-simp")
                 (commit "b73df7fc0994912ce785462b3be569ae81258ac2")))
           (file-name "rime-pinyin-simp-checkout")
           (sha256
            (base32
             "1m9hchnj1xf5s5185qm66ja0g1324drc98b2jjhnqgcp47bwz9fx"))))
       ("rime-prelude"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-prelude")
                 (commit "8a52b4f86a59f3eb602f9a4cf6a680a67c15df8c")))
           (file-name "rime-prelude-checkout")
           (sha256
            (base32
             "039fr3996vfxzn2milaq1f5fw08f6zgjsxsql6cfhsc5b55fidm7"))))
       ("rime-quick"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-quick")
                 (commit "3fe5911ba608cb2df1b6301b76ad1573bd482a76")))
           (file-name "rime-quick-checkout")
           (sha256
            (base32
             "08bh87ym5qvw55lyw20l3m7jd4c2z5rvil8h5q8790r7z6j6ijy9"))))
       ("rime-scj"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-scj")
                 (commit "cab5a0858765eff0553dd685a2d61d5536e9149c")))
           (file-name "rime-scj-checkout")
           (sha256
            (base32
             "0ard2bjp4896a8dimmcwyjwgmp9kl4rz92yc92jnd3y4rgwl6fvk"))))
       ("rime-soutzoe"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-soutzoe")
                 (commit "beeaeca72d8e17dfd1e9af58680439e9012987dc")))
           (file-name "rime-soutzoe-checkout")
           (sha256
            (base32
             "0jyqx0q9s0qxn168l5n8zav8jcl2g5ppr7pa8jm1vwrllf20slcc"))))
       ("rime-stenotype"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-stenotype")
                 (commit "f3e9189d5ce33c55d3936cc58e39d0c88b3f0c88")))
           (file-name "rime-stenotype-checkout")
           (sha256
            (base32
             "0dl6px7lrh3xa87knjzwzdcwjj1k1dg4l72q7lb48an4s9f1cy5d"))))
       ("rime-stroke"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-stroke")
                 (commit "ea8576d1accd6fda339e96b415caadb56e2a07d1")))
           (file-name "rime-stroke-checkout")
           (sha256
            (base32
             "07h6nq9867hjrd2v3h1pnr940sdrw4mqrzj43siz1rzjxz3s904r"))))
       ("rime-terra-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-terra-pinyin")
                 (commit "492aaf914f9de37cc9d26b846dc693116de70ae8")))
           (file-name "rime-terra-pinyin-checkout")
           (sha256
            (base32
             "1l4l2w42mc3sf7jwbadx95gzrsq11ld9f6yj2hwaq9accainw3bf"))))
       ("rime-wubi"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-wubi")
                 (commit "dd052ee158a38cb791755318b1aef9b4a3ed0316")))
           (file-name "rime-wubi-checkout")
           (sha256
            (base32
             "00xzv3sbwqh2jz4i7s315h7rw17qa2dgj7kflyy3blxk0s2cqiqa"))))
       ("rime-wugniu"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-wugniu")
                 (commit "abd1ee98efbf170258fcf43875c21a4259e00b61")))
           (file-name "rime-wugniu-checkout")
           (sha256
            (base32
             "0qn54d3cclny106ixdw08r5n6wn52ffs1hgrma3k0j4pv0kr9nlq"))))))
    (home-page "https://rime.im/")
    (synopsis "Schema data of Rime Input Method Engine")
    (description "@dfn{rime-data} provides the schema data of Rime Input
Method Engine.")
    (license lgpl3)))

(define-public ibus-rime
  (package
    (name "ibus-rime")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/ibus-rime")
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
         (add-after 'unpack 'fix-file-names
           (lambda* (#:key outputs #:allow-other-keys)
             ;; IBus uses the component file rime.xml to start the Rime
             ;; engine.  It must be patched with appropriate file names.
             (substitute* "rime.xml"
               (("/usr") (assoc-ref outputs "out")))
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
     `(("cmake" ,cmake-minimal)
       ("pkg-config" ,pkg-config)))
    (home-page "https://rime.im/")
    (synopsis "Rime Input Method Engine for IBus")
    (description "@dfn{ibus-rime} provides the Rime input method engine for
IBus.  Rime is a lightweight, extensible input method engine supporting
various input schemas including glyph-based input methods, romanization-based
input methods as well as those for Chinese dialects.  It has the ability to
compose phrases and sentences intelligently and provide very accurate
traditional Chinese output.")
    (license gpl3)))

(define-public libhangul
  (package
    (name "libhangul")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://kldp.net/hangul/release/"
                           "3442-libhangul-" version ".tar.gz"))
       (sha256
        (base32
         "0ni9b0v70wkm0116na7ghv03pgxsfpfszhgyj3hld3bxamfal1ar"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/libhangul/libhangul")
    (synopsis "Library to support hangul input method logic")
    (description
     "This package provides a library to support hangul input method logic,
hanja dictionary and small hangul character classification.")
    (license lgpl2.1+)))

(define-public ibus-libhangul
  (package
    (name "ibus-libhangul")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libhangul/ibus-hangul/"
                           "releases/download/" version
                           "/ibus-hangul-" version ".tar.gz"))
       (sha256
        (base32
         "1400ba2p34vr9q285lqvjm73f6m677cgfdymmjpiwyrjgbbiqrjy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/libexec/ibus-setup-hangul")
               `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))
               `("LD_LIBRARY_PATH" ":" prefix
                 (,(string-append (assoc-ref inputs "libhangul") "/lib")))
               `("GI_TYPELIB_PATH" ":" prefix
                 (,(getenv "GI_TYPELIB_PATH"))))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")))
    (inputs
     `(("ibus" ,ibus)
       ("glib" ,glib)
       ("python-pygobject" ,python-pygobject)
       ("gtk+" ,gtk+)
       ("libhangul" ,libhangul)
       ("python" ,python)))
    (home-page "https://github.com/libhangul/ibus-hangul")
    (synopsis "Hangul engine for IBus")
    (description
     "ibus-hangul is a Korean input method engine for IBus.")
    (license gpl2+)))
