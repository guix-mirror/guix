;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2019, 2020 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
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

(define-module (gnu packages jami)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define %jami-version "20200401.1.6f090de")

(define* (jami-source #:key without-daemon)
  (origin
    (method url-fetch)
    (uri (string-append "https://dl.jami.net/release/tarballs/jami_"
                        %jami-version
                        ".tar.gz"))
    (modules '((guix build utils)))
    (snippet
     (if without-daemon
         '(begin
            (delete-file-recursively "daemon/contrib"))
         #f))
    (sha256
     (base32
      "0lryx9n1jn0jsw7s10pbwivqv0d5m3jdzhdhdyg5n02v72mjvkmh"))))

;; Savoir-Faire Linux modifies many libraries to add features
;; to Jami. This procedure makes applying patches to a given
;; package easy.
(define jami-apply-dependency-patches
  '(lambda* (#:key inputs dep-name patches)
     (let ((patches-directory "sfl-patches"))
       (mkdir-p patches-directory)
       (invoke "tar" "-xvf" (assoc-ref inputs "sfl-patches")
               "-C" patches-directory
               "--strip-components=5"
               (string-append "ring-project/daemon/contrib/src/"
                              dep-name))
       (for-each
        (lambda (file)
          (invoke "patch" "--force" "-p1" "-i"
                  (string-append patches-directory "/"
                                 file ".patch")))
        patches))))

(define-public pjproject-jami
  (package
    (inherit pjproject)
    (name "pjproject-jami")
    (native-inputs
     `(("sfl-patches" ,(jami-source))
       ,@(package-native-inputs pjproject)))
    (arguments
     `(#:tests? #f
       ;; See ring-project/daemon/contrib/src/pjproject/rules.mak.
       #:configure-flags
       (list "--disable-oss"
             "--disable-sound"
             "--disable-video"
             ;; The following flag is Linux specific.
             ,@(if (hurd-triplet? (or (%current-system)
                                      (%current-target-system)))
                   '()
                   '("--enable-epoll"))
             "--enable-ext-sound"
             "--disable-speex-aec"
             "--disable-g711-codec"
             "--disable-l16-codec"
             "--disable-gsm-codec"
             "--disable-g722-codec"
             "--disable-g7221-codec"
             "--disable-speex-codec"
             "--disable-ilbc-codec"
             "--disable-opencore-amr"
             "--disable-silk"
             "--disable-sdl"
             "--disable-ffmpeg"
             "--disable-v4l2"
             "--disable-openh264"
             "--disable-resample"
             "--disable-libwebrtc"
             "--with-gnutls"
             "--with-external-srtp"
             ;; We need -fPIC or else we get the following error when linking
             ;; against pjproject-jami:
             ;;   relocation R_X86_64_32S against `.rodata' can not be used when
             ;;   making a shared object;
             "CFLAGS=-fPIC"
             "CXXFLAGS=-fPIC")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t))
         (add-after 'unpack 'apply-patches
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jami-apply-dependency-patches ,jami-apply-dependency-patches))
               ;; Comes from
               ;; "ring-project/daemon/contrib/src/pjproject/rules.mak".
               ;; WARNING: These amount for huge changes in pjproject.
               (jami-apply-dependency-patches
                #:inputs inputs
                #:dep-name "pjproject"
                #:patches
                '("0001-rfc6544"
                  "0002-rfc2466"
                  "0003-add-tcp-keep-alive"
                  "0004-multiple_listeners"
                  "0005-fix_ebusy_turn"
                  "0006-ignore_ipv6_on_transport_check"
                  "0007-pj_ice_sess"
                  "0008-fix_ioqueue_ipv6_sendto"
                  "0009-add-config-site"))
               #t)))
         ;; TODO: We could use substitute-keyword-arguments instead of
         ;; repeating the phases from pjproject, but somehow it does
         ;; not work.
         (add-before 'build 'build-dep
           (lambda _ (invoke "make" "dep")))
         (add-before 'patch-source-shebangs 'autoconf
           (lambda _
             (invoke "autoconf" "-v" "-f" "-i" "-o"
                     "aconfigure" "aconfigure.ac")))
         (add-before 'autoconf 'disable-some-tests
           ;; Three of the six test programs fail due to missing network
           ;; access.
           (lambda _
             (substitute* "Makefile"
               (("selftest: pjlib-test pjlib-util-test pjnath-test pjmedia-test pjsip-test pjsua-test")
                "selftest: pjlib-test pjlib-util-test pjmedia-test"))
             #t)))))))

(define-public libring
  (package
    (name "libring")
    (version %jami-version)
    (source (jami-source #:without-daemon #t))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("dbus-c++" ,dbus-c++)
       ("eudev" ,eudev)
       ("ffmpeg" ,ffmpeg)
       ("flac" ,flac)
       ("gmp" ,gmp)
       ("gsm" ,gsm)
       ("jack" ,jack-1)
       ("jsoncpp" ,jsoncpp)
       ("libnatpmp" ,libnatpmp)
       ("libogg" ,libogg)
       ("libva" ,libva)
       ("opendht" ,opendht)
       ("opus" ,opus)
       ("pcre" ,pcre)
       ("pulseaudio" ,pulseaudio)
       ("libsamplerate" ,libsamplerate)
       ("libsndfile" ,libsndfile)
       ("speex" ,speex)
       ("speexdsp" ,speexdsp)
       ("libupnp" ,libupnp)
       ("libvorbis" ,libvorbis)
       ("libx264" ,libx264)
       ("libvdpau" ,libvdpau)
       ("yaml-cpp" ,yaml-cpp)
       ("zlib" ,zlib)
       ("openssl" ,openssl)
       ("libsecp256k1" ,libsecp256k1)
       ("python" ,python)
       ("python-wrapper" ,python-wrapper)
       ("restinio" ,restinio)
       ("libx11" ,libx11)
       ("asio" ,asio)
       ;; TODO: Upstream seems to rely on a custom pjproject (a.k.a. pjsip) version.
       ;; See https://git.jami.net/savoirfairelinux/ring-daemon/issues/24.
       ("pjproject" ,pjproject-jami)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("which" ,which)
       ("cppunit" ,cppunit)
       ("perl" ,perl)))                 ; Needed for documentation.
    (arguments
     `(#:tests? #f         ; The tests fail to compile due to missing headers.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "daemon")
             #t))
         (add-before 'build 'add-lib-dir
           (lambda _
             (mkdir-p "src/lib")
             #t)))))
    (synopsis "Distributed multimedia communications platform")
    (description "Jami (formerly GNU Ring) is a secure and distributed voice,
video and chat communication platform that requires no centralized server and
leaves the power of privacy in the hands of the user.  It supports the SIP and
IAX protocols, as well as decentralized calling using P2P-DHT.

This package provides a library and daemon implementing the Jami core
functionality.")
    (home-page "https://jami.net/")
    (license license:gpl3+)))

(define-public libringclient
  (package
    (inherit libring)
    (name "libringclient")
    (build-system cmake-build-system)
    (propagated-inputs
     `(("libring" ,libring)     ; For 'dring'.
       ("qtbase" ,qtbase)       ; Qt is included in several installed headers.
       ("qttools" ,qttools)))
    (arguments
     `(#:tests? #f                      ; There is no testsuite.
       #:configure-flags
       (list (string-append "-DRING_BUILD_DIR="
                            (assoc-ref %build-inputs "libring") "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "lrc")
             #t))
         (add-before 'configure 'fix-dbus-interfaces-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$\\{CMAKE_INSTALL_PREFIX\\}(/share/dbus-1/interfaces)" _ dbus-interfaces-path-suffix)
                (string-append (assoc-ref inputs "libring")
                               dbus-interfaces-path-suffix))))))))
    (synopsis "Distributed multimedia communications platform")
    (description "Jami (formerly GNU Ring) is a secure and distributed voice,
video and chat communication platform that requires no centralized server and
leaves the power of privacy in the hands of the user.  It supports the SIP and
IAX protocols, as well as decentralized calling using P2P-DHT.

This package provides a library common to all Jami clients.")
    (home-page "https://jami.net")
    (license license:gpl3+)))

(define-public jami
  (package
    (inherit libring)
    (name "jami")
    (build-system cmake-build-system)
    (inputs
     `(("libringclient" ,libringclient)
       ("gtk+" ,gtk+)
       ("qrencode" ,qrencode)
       ("libnotify" ,libnotify)
       ("clutter" ,clutter)
       ("clutter-gtk" ,clutter-gtk)
       ("libcanberra" ,libcanberra)
       ("webkitgtk" ,webkitgtk)
       ;; TODO: We must wrap ring-client-gnome to force using the
       ;; `sqlite-with-column-metadata' package instead of `sqlite' or else it
       ;; fails with:
       ;;
       ;;   /gnu/store/...-qtbase-5.11.2/lib/qt5/plugins/sqldrivers/libqsqlite.so:
       ;;   undefined symbol: sqlite3_column_table_name16
       ;;
       ;; qtbase is built against sqlite-with-column-metadata but somehow
       ;; jami-client-gnome ends up with both `sqlite' and
       ;; `sqlite-with-column-metadata' as inputs and it seems that
       ;; libqsqlite.so gets confused.
       ("sqlite" ,sqlite-with-column-metadata)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("doxygen" ,doxygen)))
    (propagated-inputs
     `(("libring" ,libring) ; Contains `dring', the daemon, which is automatically by d-bus.
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ("evolution-data-server" ,evolution-data-server)))
    (arguments
     `(#:tests? #f                      ; There is no testsuite.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "client-gnome")
             #t))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (path (string-append (assoc-ref inputs "sqlite") "/lib")))
               (wrap-program (string-append out "/bin/jami-gnome")
                 `("LD_LIBRARY_PATH" ":" prefix (,path))))
             #t)))))
    (synopsis "Distributed, privacy-respecting communication program")
    (description "Jami (formerly GNU Ring) is a secure and distributed voice,
video and chat communication platform that requires no centralized server and
leaves the power of privacy in the hands of the user.  It supports the SIP and
IAX protocols, as well as decentralized calling using P2P-DHT.

This package provides the Jami client for the GNOME desktop.")
    (home-page "https://jami.net")
    (license license:gpl3+)))

(define-public jami-client-gnome
  (deprecated-package "jami-client-gnome" jami))
