;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
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

(define-module (gnu packages telegram)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages animation)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages fcitx)
  #:use-module (gnu packages fcitx5)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system qt))

(define-public webrtc-for-telegram-desktop
  (let ((commit "fa86fcc00c218813d61a272a56feab55c76a1ab9")
        (revision "52"))
    (hidden-package
     (package
       (name "webrtc-for-telegram-desktop")
       (version
        (git-version "0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/desktop-app/tg_owt.git")
            (commit commit)))
          (file-name
           (git-file-name name version))
          (sha256
           (base32 "06gcrlym6vqqw7zlds9lpwyg37d5m81d87h16aps19v0v9gzan0l"))
          (modules '((guix build utils)
                     (ice-9 ftw)
                     (srfi srfi-1)))
          (snippet
           `(begin
              (let ((keep
                     '( ;; Custom forks which are incompatible with the ones in Guix.
                       "abseil-cpp" "libsrtp" "openh264" "rnnoise"
                       ;; Not available in Guix.
                       "pffft" "usrsctp"
                       ;; Has cmake support files for libvpx input.
                       "libvpx")))
                (with-directory-excursion "src/third_party"
                  (for-each delete-file-recursively
                            (lset-difference string=?
                                             (scandir ".")
                                             (cons* "." ".." keep))))
                #t)))))
       (build-system cmake-build-system)
       (arguments
        `(#:tests? #f                   ; No target
          #:configure-flags
          (list
           "-DCMAKE_C_FLAGS=-fPIC"
           "-DCMAKE_CXX_FLAGS=-fPIC")
          #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'copy-inputs
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((libvpx-from (assoc-ref inputs "libvpx"))
                       (libyuv-from (assoc-ref inputs "libyuv"))
                       (libvpx-to (string-append (getcwd)
                                                 "/src/third_party/libvpx/source/libvpx"))
                       (libyuv-to (string-append (getcwd)
                                                 "/src/third_party/libyuv")))
                  (copy-recursively libvpx-from libvpx-to)
                  (copy-recursively libyuv-from libyuv-to))
                #t)))))
       (native-inputs
        `(("pkg-config" ,pkg-config)
          ("python" ,python-wrapper)
          ("yasm" ,yasm)))
       (inputs
        `(("alsa" ,alsa-lib)
          ("ffmpeg" ,ffmpeg)
          ("libjpeg" ,libjpeg-turbo)
          ("libvpx"
           ,(origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://chromium.googlesource.com/webm/libvpx")
                (commit "5b63f0f821e94f8072eb483014cfc33b05978bb9")))
              (file-name
               (git-file-name "libvpx-for-webrtc-for-telegram-desktop" version))
              (sha256
               (base32 "1psvxaddihlw1k5n0anxif3qli6zyw2sa2ywn6mkb8six9myrp68"))))
          ("libyuv"
           ,(origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://chromium.googlesource.com/libyuv/libyuv")
                (commit "ad890067f661dc747a975bc55ba3767fe30d4452")))
              (file-name
               (git-file-name "libyuv-for-webrtc-for-telegram-desktop" version))
              (sha256
               (base32 "01knnk4h247rq536097n9n3s3brxlbby3nv3ppdgsqfda3k159ll"))))
          ("openssl" ,openssl)
          ("opus" ,opus)
          ("protobuf" ,protobuf)
          ("pulseaudio" ,pulseaudio)))
       (synopsis "WebRTC support for Telegram Desktop")
       (description "WebRTC-for-Telegram-Desktop is a custom WebRTC fork by
Telegram project, for its use in telegram desktop client.")
       (home-page "https://github.com/desktop-app/tg_owt")
       (license
        (list
         ;; Abseil-CPP
         license:asl2.0
         ;; LibYuv
         (license:non-copyleft "file:///src/third_party/libyuv/LICENSE")
         ;; OpenH264
         license:bsd-2
         ;; PFFFT
         (license:non-copyleft "file:///src/third_party/pffft/LICENSE")
         ;; RnNoise
         license:gpl3
         ;; LibSRTP, LibVPx, UsrSCTP and Others
         license:bsd-3))))))

(define-public rlottie-for-telegram-desktop
  (let ((commit "cbd43984ebdf783e94c8303c41385bf82aa36d5b")
        (revision "671"))
    (hidden-package
     (package
       (inherit rlottie)
       (version
        (git-version "0.0.1" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/desktop-app/rlottie.git")
            (commit commit)))
          (file-name
           (git-file-name "rlottie-for-telegram-desktop" version))
          (sha256
           (base32 "1lxpbgbhps9rmck036mgmiknqrzpjxpas8n7qxykv6pwzn0c8n0c"))))
       (arguments
        `(#:configure-flags
          (list
           "-Dlog=true"
           "-Ddumptree=true"
           "-Dtest=true")
          #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'patch-cxx-flags
              (lambda _
                (substitute* "meson.build"
                  (("werror=true")
                   "werror=false"))
                #t)))))))))

(define-public libtgvoip-for-telegram-desktop
  (let ((commit "37d98e984fd6fa389262307db826d52ab86c8241")
        (revision "87"))
    (hidden-package
     (package
       (inherit libtgvoip)
       (version
        (git-version "2.4.4" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/telegramdesktop/libtgvoip.git")
            (commit commit)))
          (file-name
           (git-file-name "libtgvoip-for-telegram-desktop" version))
          (sha256
           (base32 "13dzvyq8p20nlhjihv18aj6y97czk07qdl0k6v81vp6mbwcldg7h"))))
       (arguments
        `(#:configure-flags
          (list
           "--disable-static"
           "--disable-dsp"              ; FIXME
           "--enable-audio-callback"
           "--with-alsa"
           "--with-pulse")
          #:phases
          (modify-phases %standard-phases
            (add-after 'unpack 'patch-linkers
              (lambda _
                (substitute* "Makefile.am"
                  (("\\$\\(CRYPTO_LIBS\\) \\$\\(OPUS_LIBS\\)")
                   "$(CRYPTO_LIBS) $(OPUS_LIBS) $(ALSA_LIBS) $(PULSE_LIBS)"))
                (substitute* "tgvoip.pc.in"
                  (("libcrypto opus")
                   "libcrypto opus alsa libpulse"))
                #t)))))
       (native-inputs
        `(("autoconf" ,autoconf)
          ("automake" ,automake)
          ("libtool" ,libtool)
          ("pkg-config" ,pkg-config)))))))

(define-public telegram-desktop
  (package
    (name "telegram-desktop")
    (version "2.5.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/telegramdesktop/tdesktop.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0drirhkr9gnm1g03lcqmvap5ljlk859c29gbsm63hhsgv15dlw0y"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        `(begin
           (let ((keep
                  '( ;; Not available in Guix.
                    "SPMediaKeyTap" "statusnotifieritem" "tgcalls")))
             (with-directory-excursion "Telegram/ThirdParty"
               (for-each delete-file-recursively
                         (lset-difference string=?
                                          (scandir ".")
                                          (cons* "." ".." keep))))
             #t)))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:imported-modules
       (,@%qt-build-system-modules
        (guix build glib-or-gtk-build-system))
       #:modules
       ((guix build qt-build-system)
        ((guix build glib-or-gtk-build-system)
         #:prefix glib-or-gtk:)
        (guix build utils)
        (ice-9 match))
       #:configure-flags
       (list
        ;; Client applications must provide their own API-ID and API-HASH,
        ;; see also <https://core.telegram.org/api/obtaining_api_id>.
        ;; In case, that the credentials below fail to work, contact
        ;;   Raghav Gururajan <rg@raghavgururajan.name>
        "-DTDESKTOP_API_ID=2791056"
        "-DTDESKTOP_API_HASH=582d6d0b44f7a2de949e99271fd8b3f2"
        ;; Use bundled fonts as fallback.
        "-DDESKTOP_APP_USE_PACKAGED_FONTS=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-after 'make-writable 'copy-inputs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (for-each
              (match-lambda
                ((dst src)
                 (copy-recursively src dst)
                 (for-each make-file-writable (find-files dst))))
              `(("cmake" ,(assoc-ref inputs "cmake-helpers"))
                ("Telegram/codegen" ,(assoc-ref inputs "codegen-source"))
                ("Telegram/lib_base" ,(assoc-ref inputs "lib-base-source"))
                ("Telegram/lib_crl" ,(assoc-ref inputs "lib-crl-source"))
                ("Telegram/lib_lottie"
                 ,(assoc-ref inputs "lib-lottie-source"))
                ("Telegram/lib_qr" ,(assoc-ref inputs "lib-qr-source"))
                ("Telegram/lib_rlottie"
                 ,(assoc-ref inputs "lib-rlottie-source"))
                ("Telegram/lib_rpl" ,(assoc-ref inputs "lib-rpl-source"))
                ("Telegram/lib_spellcheck"
                 ,(assoc-ref inputs "lib-spellcheck-source"))
                ("Telegram/lib_storage"
                 ,(assoc-ref inputs "lib-storage-source"))
                ("Telegram/lib_tl" ,(assoc-ref inputs "lib-tl-source"))
                ("Telegram/lib_ui" ,(assoc-ref inputs "lib-ui-source"))
                ("Telegram/lib_webrtc" ,(assoc-ref inputs "lib-webrtc-source"))
                ("Telegram/ThirdParty/tgcalls"
                 ,(assoc-ref inputs "tgcalls-source"))))
             #t))
         (add-before 'configure 'patch-cxx-flags
           (lambda _
             (substitute* "cmake/options_linux.cmake"
               (("class-memaccess") "all"))
             #t))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     `(("cmake-helpers"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/cmake_helpers.git")
             (commit "a81345a28d407fb5acd5267ec6afa1864f4e8d5b")))
           (file-name
            (git-file-name "cmake-helpers-for-telegram-desktop" version))
           (sha256
            (base32 "0s5hxip68dmkaspjq6j30wx1r5v4prnrjza79hdbznz6i57a2248"))))
       ("cmake-shared" ,cmake-shared)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("gcc" ,gcc-9)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("qttools" ,qttools)))
    (inputs
     `(("alsa" ,alsa-lib)
       ("c++-gsl" ,c++-gsl)
       ("catch" ,catch-framework2)
       ("codegen-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/codegen.git")
             (commit "127968de8129e8ccfa6ac50721c70415a5a087c3")))
           (file-name
            (git-file-name "codegen" version))
           (sha256
            (base32 "036hzjrsk134ky62192nra43rsln5kh5gz20q1920s922661zky2"))))
       ("expected" ,libexpected)
       ("fcitx-qt5" ,fcitx-qt5)
       ("fcitx5-qt" ,fcitx5-qt)
       ("ffmpeg" ,ffmpeg)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("hime" ,hime)
       ("hunspell" ,hunspell)
       ("iconv" ,libiconv)
       ("lib-base-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_base.git")
             (commit "81df0d0b7842be2b6c88f93dfa136b8efea4c9ad")))
           (file-name
            (git-file-name "lib-base-for-telegram-desktop" version))
           (sha256
            (base32 "0ikddprjnjvg0ic8jr2886xq0f18syp587q6z2kci9xmdnvjl217"))))
       ("lib-crl-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_crl.git")
             (commit "16150bf71d79382738114b913f137ec1a1a7630c")))
           (file-name
            (git-file-name "lib-crl-for-telegram-desktop" version))
           (sha256
            (base32 "0qhagdr26aqb9w7wnchcmk1j7ln28x3wbkkkm06b8h0mybksbj7q"))))
       ("lib-lottie-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_lottie.git")
             (commit "fb40f379d82ffa1fc7506e9a8dddcf48847715ae")))
           (file-name
            (git-file-name "lib-lottie-for-telegram-desktop" version))
           (sha256
            (base32 "1vq0mqxcrrv7akcqk9cl4mm61zw6dcfmy8adl0pcp49kynm64saw"))))
       ("lib-qr-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_qr.git")
             (commit "92ce41a690a463eb462089a4eb1e51e019308018")))
           (file-name
            (git-file-name "lib-qr-for-telegram-desktop" version))
           (sha256
            (base32 "182939nv7xs9b3bgah3gl5y9hx5r59mabd2jw3z6717vc96qi2pj"))))
       ("lib-rlottie-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_rlottie.git")
             (commit "0671bf70547381effcf442ec9618e04502a8adbc")))
           (file-name
            (git-file-name "lib-rlottie-for-telegram-desktop" version))
           (sha256
            (base32 "05qnza7j15356s8jq16pkbyp4zr586lssmd86lz5jq23lcb3raxv"))))
       ("lib-rpl-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_rpl.git")
             (commit "e1b96399d9031c4ef0354631e6bb375029d29d9f")))
           (file-name
            (git-file-name "lib-rpl-for-telegram-desktop" version))
           (sha256
            (base32 "1wvqazljd2kq1fxlj250jhjrig529499bym9p81dx33kh1l9dgss"))))
       ("lib-spellcheck-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_spellcheck.git")
             (commit "1b540b38ed78e9a3cba93e9ba4ce4525ab692277")))
           (file-name
            (git-file-name "lib-spellcheck-for-telegram-desktop" version))
           (sha256
            (base32 "0a7042h5zrdvgs7v153ral2dh1zj84di5yjcmgcry5k4s1im9di7"))))
       ("lib-storage-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_storage.git")
             (commit "cbe51722b73cfa9ff27bd59294b08aa5ee33c936")))
           (file-name
            (git-file-name "lib-storage-for-telegram-desktop" version))
           (sha256
            (base32 "045l5xsyagyz17gbhmmvl2miss4nb92p0dmza7yfs9pkg9gs0f87"))))
       ("lib-tl-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_tl.git")
             (commit "404c83d77e5edb8a39f8e9f56a6340960fe5070e")))
           (file-name
            (git-file-name "lib-tl-for-telegram-desktop" version))
           (sha256
            (base32 "1k34nkvvcjqw5q81n1qmklid60cvzjk4lmn9qjimk437m6wbii7f"))))
       ("lib-ui-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_ui.git")
             (commit "1e2799245cf2720a329ecb5cf5644fded669cce6")))
           (file-name
            (git-file-name "lib-ui-for-telegram-desktop" version))
           (sha256
            (base32 "0kd4njcvic2700f00qn25vn3b80vsd2flsm3pi2synnldkiy8lcw"))))
       ("lib-webrtc-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/desktop-app/lib_webrtc.git")
             (commit "4bc51d6f6d5740159fdb51cb1593e80ce149ed4e")))
           (file-name
            (git-file-name "lib-webrtc-for-telegram-desktop" version))
           (sha256
            (base32 "06hpyq4qglrj3cb1xg8ghlmzm9ra8f5n6vm7hcy67n2wk8sy4cal"))))
       ("libdbusmenu-qt" ,libdbusmenu-qt)
       ("libjpeg" ,libjpeg-turbo)
       ("libtgvoip" ,libtgvoip-for-telegram-desktop)
       ("lz4" ,lz4)
       ;; TODO: Uncomment materialdecoration if build successfully.
       ;; ("materialdecoration" ,materialdecoration)
       ("minizip" ,minizip)
       ("nimf" ,nimf)
       ("openal" ,openal)
       ("openssl" ,openssl)
       ("opus" ,opus)
       ("pulseaudio" ,pulseaudio)
       ("qrcodegen" ,qrcodegen-cpp)
       ("qt" ,qtbase)
       ("qt5ct" ,qt5ct)
       ("qtimageformats" ,qtimageformats)
       ("qtwayland" ,qtwayland)
       ("range-v3" ,range-v3)
       ("rlottie" ,rlottie-for-telegram-desktop)
       ("tgcalls-source"
        ,(origin
           (method git-fetch)
           (uri
            (git-reference
             (url "https://github.com/TelegramMessenger/tgcalls.git")
             (commit "178983f72312ca8bd422bc73810fd63f1a89bd9d")))
           (file-name
            (git-file-name "tgcalls-for-telegram-desktop" version))
           (sha256
            (base32 "1xad65c9m6mj6zdj08flafvh8xjkd7xi9r1agcyc64y69lr427d0"))))
       ("webrtc" ,webrtc-for-telegram-desktop)
       ("x11" ,libx11)
       ("xcb" ,libxcb)
       ("xcb-keysyms" ,xcb-util-keysyms)
       ("xxhash" ,xxhash)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("dconf" ,dconf)))
    (synopsis "Telegram Desktop")
    (description "Telegram desktop is the official desktop version of the
Telegram instant messager.")
    (home-page "https://desktop.telegram.org/")
    (license
     (list
      ;; ThirdParty
      license:lgpl2.1+
      ;; Others
      license:gpl3+))))

(define-public tl-parser
  (let ((commit "1933e76f8f4fb74311be723b432e4c56e3a5ec06")
        (revision "21"))
    (package
      (name "tl-parser")
      (version
       (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/vysheng/tl-parser.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "13cwi247kajzpkbl86hnwmn1sn2h6rqndz6khajbqj0mlw9mv4hq"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%cmake-build-system-modules)
         #:modules
         (((guix build copy-build-system)
           #:prefix copy:)
          (guix build cmake-build-system)
          (guix build utils))
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda args
               (apply (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      '(("." "bin"
                         #:include ("tl-parser"))
                        ("../source" "include/tl-parser"
                         #:include-regexp ("\\.h$")))
                      args))))))
      (synopsis "Parse tl scheme to tlo")
      (description "TL-Parser is a tl scheme to tlo file parser.  It was formely
a part of telegram-cli, but now being maintained separately.")
      (home-page "https://github.com/vysheng/tl-parser")
      (license license:gpl2+))))

(define-public tgl
  (let ((commit "ffb04caca71de0cddf28cd33a4575922900a59ed")
        (revision "181"))
    (package
      (name "tgl")
      (version
       (git-version "2.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/vysheng/tgl.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "0cf5s7ygslb5klg1qv9qdc3hivhspmvh3zkacyyhd2yyikb5p0f9"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%gnu-build-system-modules)
         #:modules
         (((guix build copy-build-system)
           #:prefix copy:)
          (guix build gnu-build-system)
          (guix build utils))
         #:configure-flags
         (list
          ;; Use gcrypt instead of openssl.
          "--disable-openssl"
          ;; Enable extended queries system.
          "--enable-extf"
          ;; Include libevent-based net and timers.
          "--enable-libevent")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'trigger-bootstrap
             (lambda _
               (delete-file "configure")
               #t))
           (add-after 'trigger-bootstrap 'patch-tl-parser
             (lambda _
               (delete-file "Makefile.tl-parser")
               (substitute* "Makefile.in"
                 (("include \\$\\{srcdir\\}/Makefile\\.tl-parser")
                  "")
                 (("\\$\\{EXE\\}/tl-parser")
                  "tl-parser"))
               #t))
           (replace 'install
             (lambda args
               (apply (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      '(("bin" "bin")
                        ("." "include/tgl"
                         #:include-regexp ("\\.h$"))
                        ("libs" "lib/tgl"))
                      args))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("libevent" ,libevent)
         ("libgcrypt" ,libgcrypt)
         ("tl-parser" ,tl-parser)
         ("zlib" ,zlib)))
      (synopsis "Telegram Library")
      (description "TGL is the telegram library for telegram-cli.")
      (home-page "https://github.com/vysheng/tgl")
      (license license:lgpl2.1+))))

(define-public telegram-cli
  (let ((commit "6547c0b21b977b327b3c5e8142963f4bc246187a")
        (revision "324"))
    (package
      (name "telegram-cli")
      (version
       (git-version "1.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/vysheng/tg.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "0c1w7jgska71jjbvg1y09v52549pwa4zkdjly18yxywn7gayd2p6"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%gnu-build-system-modules)
         #:modules
         (((guix build copy-build-system)
           #:prefix copy:)
          (guix build gnu-build-system)
          (guix build utils))
         #:configure-flags
         (list
          ;; Use gcrypt instead of openssl.
          "--disable-openssl")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'trigger-bootstrap
             (lambda _
               (delete-file "configure")
               #t))
           (add-after 'trigger-bootstrap 'patch-tgl-and-tlparser
             (lambda* (#:key inputs #:allow-other-keys)
               (for-each delete-file
                         (list
                          "Makefile.tgl"
                          "Makefile.tl-parser"))
               (substitute* "Makefile.in"
                 (("include \\$\\{srcdir\\}/Makefile\\.tl-parser")
                  "")
                 (("include \\$\\{srcdir\\}/Makefile\\.tgl")
                  "")
                 (("-I\\$\\{srcdir\\}/tgl")
                  (string-append "-I" (assoc-ref inputs "tgl")
                                 "/include/tgl"))
                 (("AUTO=auto")
                  (string-append "AUTO=" (assoc-ref inputs "tgl")
                                 "/include/tgl/auto"))
                 (("LIB=libs")
                  (string-append "LIB=" (assoc-ref inputs "tgl")
                                 "/lib/tgl")))
               #t))
           (replace 'install
             (lambda args
               (apply (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      '(("bin" "bin")
                        ("." "etc/telegram-cli"
                         #:include-regexp ("\\.pub$")
                         #:exclude ("tg-server.pub")))
                      args))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("jansson" ,jansson)
         ("libconfig" ,libconfig)
         ("libevent" ,libevent)
         ("libgcrypt" ,libgcrypt)
         ("lua" ,lua)
         ("openssl" ,openssl)
         ("perl" ,perl)
         ("python" ,python)
         ("readline" ,readline)
         ("tgl" ,tgl)
         ("tl-parser" ,tl-parser)
         ("zlib" ,zlib)))
      (synopsis "Telegram Messenger CLI")
      (description "TG is the command-line interface for Telegram Messenger.")
      (home-page "https://github.com/vysheng/tg")
      (license license:gpl2+))))
