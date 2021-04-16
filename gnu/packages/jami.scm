;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2019, 2020 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
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
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define %jami-version "20210326.1.cfba013")

(define* (jami-source #:key keep-contrib-patches?)
  "Return an origin object of the tarball release sources archive of Jami.
When KEEP-CONTRIB-PATCHES? is #t, do not completely remove the contrib
subdirectory, which contains patches to be applied to some of the dependencies
of Jami."
  (origin
    (method url-fetch)
    (uri (string-append "https://dl.jami.net/release/tarballs/jami_"
                        %jami-version
                        ".tar.gz"))
    (modules '((guix build utils)))
    (snippet
     `(begin
        ;; Delete multiple MiBs of bundled tarballs.  The contrib directory
        ;; contains the custom patches for pjproject and other libraries used
        ;; by Savoir-faire Linux.
        (if ,keep-contrib-patches?
            (delete-file-recursively "daemon/contrib/tarballs")
            (delete-file-recursively "daemon/contrib"))
        ;; Remove code from unused Jami clients.
        (for-each delete-file-recursively '("client-android"
                                            "client-electron"
                                            "client-ios"
                                            "client-macosx"
                                            "client-uwp"))))
    (sha256
     (base32
      "1h0avma8bdzyznkz39crjyv2888bii4f49md15jg7970dyp5pdyz"))))

(define %sfl-patches (jami-source #:keep-contrib-patches? #t))

(define %jami-sources (jami-source))

;; Savoir-faire Linux maintains a set of patches for some key dependencies
;; (currently pjproject and ffmpeg) of Jami that haven't yet been integrated
;; upstream.  This procedure simplifies the process of applying these patches.x
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
          (invoke "patch" "--force" "--ignore-whitespace" "-p1" "-i"
                  (string-append patches-directory "/"
                                 file ".patch")))
        patches))))

;;; Jami maintains pjproject patches that add the ability to do ICE over TCP,
;;; among other things.  The patches are currently based on pjproject 2.10.
(define-public pjproject-jami
  (package
    (inherit pjproject)
    (name "pjproject-jami")
    (version "2.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pjsip/pjproject")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1aklicpgwc88578k03i5d5cm5h8mfm7hmx8vfprchbmaa2p8f4z0"))
              (patches (search-patches
                        "pjproject-correct-the-cflags-field.patch"
                        "pjproject-fix-pkg-config-ldflags.patch"))))
    (native-inputs
     `(("sfl-patches" ,%sfl-patches)
       ,@(package-native-inputs pjproject)))
    (arguments
     (substitute-keyword-arguments (package-arguments pjproject)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'make-source-files-writable 'apply-patches
             (lambda* (#:key inputs #:allow-other-keys)
               (,jami-apply-dependency-patches
                #:inputs inputs
                #:dep-name "pjproject"
                #:patches
                '("0001-rfc6544"
                  "0002-rfc2466"
                  "0003-add-tcp-keep-alive"
                  "0004-multiple_listeners"
                  "0005-fix_ebusy_turn"
                  "0006-ignore_ipv6_on_transport_check"
                  "0007-upnp-srflx-nat-assisted-cand"
                  "0008-fix_ioqueue_ipv6_sendto"
                  "0009-add-config-site"
                  ;; Already taken care of via the origin patches.
                  ;;"0010-fix-pkgconfig"
                  "0011-fix-tcp-death-detection"
                  "0012-fix-turn-shutdown-crash"
                  "0013-Assign-unique-local-preferences-for-candidates-with-"
                  "0014-Add-new-compile-time-setting-PJ_ICE_ST_USE_TURN_PERM"
                  "0015-update-local-preference-for-peer-reflexive-candidate"
                  "0016-use-addrinfo-instead-CFHOST"
                  "0017-CVE-2020-15260"
                  "0018-CVE-2021-21375"))))))))))

;; The following variables are configure flags used by ffmpeg-jami.  They're
;; from the ring-project/daemon/contrib/src/ffmpeg/rules.mak file. We try to
;; keep it as close to the official Jami package as possible, to provide all
;; the codecs and extra features that are expected.
;;
;; See:
;; https://review.jami.net/plugins/gitiles/ring-daemon/+/refs/heads/master/contrib/src/ffmpeg/rules.mak

(define %ffmpeg-default-configure-flags
  '("--disable-everything"
    "--enable-zlib"
    "--enable-gpl"
    "--enable-swscale"
    "--enable-bsfs"
    "--disable-filters"
    "--disable-programs"
    "--disable-postproc"
    "--disable-protocols"
    "--enable-protocol=crypto"
    "--enable-protocol=file"
    "--enable-protocol=rtp"
    "--enable-protocol=srtp"
    "--enable-protocol=tcp"
    "--enable-protocol=udp"
    "--enable-protocol=unix"
    "--enable-protocol=pipe"

    ;; enable muxers/demuxers
    "--disable-demuxers"
    "--disable-muxers"
    "--enable-muxer=rtp"
    "--enable-muxer=g722"
    "--enable-muxer=h263"
    "--enable-muxer=h264"
    "--enable-muxer=hevc"
    "--enable-muxer=webm"
    "--enable-muxer=ogg"
    "--enable-muxer=pcm_s16be"
    "--enable-muxer=pcm_s16le"
    "--enable-demuxer=rtp"
    "--enable-demuxer=mjpeg"
    "--enable-demuxer=mjpeg_2000"
    "--enable-demuxer=mpegvideo"
    "--enable-demuxer=gif"
    "--enable-demuxer=image_jpeg_pipe"
    "--enable-demuxer=image_png_pipe"
    "--enable-demuxer=image_webp_pipe"
    "--enable-demuxer=matroska"
    "--enable-demuxer=m4v"
    "--enable-demuxer=mp3"
    "--enable-demuxer=ogg"
    "--enable-demuxer=flac"
    "--enable-demuxer=wav"
    "--enable-demuxer=ac3"
    "--enable-demuxer=g722"
    "--enable-demuxer=pcm_mulaw"
    "--enable-demuxer=pcm_alaw"
    "--enable-demuxer=pcm_s16be"
    "--enable-demuxer=pcm_s16le"
    "--enable-demuxer=h263"
    "--enable-demuxer=h264"
    "--enable-demuxer=hevc"

    ;; enable parsers
    "--enable-parser=h263"
    "--enable-parser=h264"
    "--enable-parser=hevc"
    "--enable-parser=mpeg4video"
    "--enable-parser=vp8"
    "--enable-parser=vp9"
    "--enable-parser=opus"

    ;; encoders/decoders
    "--enable-encoder=adpcm_g722"
    "--enable-decoder=adpcm_g722"
    "--enable-encoder=rawvideo"
    "--enable-decoder=rawvideo"
    "--enable-encoder=libx264"
    "--enable-decoder=h264"
    "--enable-encoder=pcm_alaw"
    "--enable-decoder=pcm_alaw"
    "--enable-encoder=pcm_mulaw"
    "--enable-decoder=pcm_mulaw"
    "--enable-encoder=mpeg4"
    "--enable-decoder=mpeg4"
    "--enable-encoder=libvpx_vp8"
    "--enable-decoder=vp8"
    "--enable-decoder=vp9"
    "--enable-encoder=h263"
    "--enable-encoder=h263p"
    "--enable-decoder=h263"
    "--enable-encoder=mjpeg"
    "--enable-decoder=mjpeg"
    "--enable-decoder=mjpegb"
    "--enable-libspeex"
    "--enable-libopus"
    "--enable-libvpx"
    "--enable-libx264"
    "--enable-encoder=libspeex"
    "--enable-decoder=libspeex"
    "--enable-encoder=libopus"
    "--enable-decoder=libopus"

    ;; decoders for ringtones and audio streaming
    "--enable-decoder=flac"
    "--enable-decoder=vorbis"
    "--enable-decoder=aac"
    "--enable-decoder=ac3"
    "--enable-decoder=eac3"
    "--enable-decoder=mp3"
    "--enable-decoder=pcm_u24be"
    "--enable-decoder=pcm_u24le"
    "--enable-decoder=pcm_u32be"
    "--enable-decoder=pcm_u32le"
    "--enable-decoder=pcm_u8"
    "--enable-decoder=pcm_f16le"
    "--enable-decoder=pcm_f24le"
    "--enable-decoder=pcm_f32be"
    "--enable-decoder=pcm_f32le"
    "--enable-decoder=pcm_f64be"
    "--enable-decoder=pcm_f64le"
    "--enable-decoder=pcm_s16be"
    "--enable-decoder=pcm_s16be_planar"
    "--enable-decoder=pcm_s16le"
    "--enable-decoder=pcm_s16le_planar"
    "--enable-decoder=pcm_s24be"
    "--enable-decoder=pcm_s24le"
    "--enable-decoder=pcm_s24le_planar"
    "--enable-decoder=pcm_s32be"
    "--enable-decoder=pcm_s32le"
    "--enable-decoder=pcm_s32le_planar"
    "--enable-decoder=pcm_s64be"
    "--enable-decoder=pcm_s64le"
    "--enable-decoder=pcm_s8"
    "--enable-decoder=pcm_s8_planar"
    "--enable-decoder=pcm_u16be"
    "--enable-decoder=pcm_u16le"

    ;; encoders/decoders for images
    "--enable-encoder=gif"
    "--enable-decoder=gif"
    "--enable-encoder=jpegls"
    "--enable-decoder=jpegls"
    "--enable-encoder=ljpeg"
    "--enable-decoder=jpeg2000"
    "--enable-encoder=png"
    "--enable-decoder=png"
    "--enable-encoder=bmp"
    "--enable-decoder=bmp"
    "--enable-encoder=tiff"
    "--enable-decoder=tiff"

    ;; filters
    "--enable-filter=scale"
    "--enable-filter=overlay"
    "--enable-filter=amix"
    "--enable-filter=amerge"
    "--enable-filter=aresample"
    "--enable-filter=format"
    "--enable-filter=aformat"
    "--enable-filter=fps"
    "--enable-filter=transpose"
    "--enable-filter=pad"))

(define %ffmpeg-linux-configure-flags
  '("--enable-pic"
    "--extra-cxxflags=-fPIC"
    "--extra-cflags=-fPIC"
    "--target-os=linux"
    "--enable-indev=v4l2"
    "--enable-indev=xcbgrab"
    "--enable-vdpau"
    "--enable-hwaccel=h264_vdpau"
    "--enable-hwaccel=mpeg4_vdpau"
    "--enable-vaapi"
    "--enable-hwaccel=h264_vaapi"
    "--enable-hwaccel=mpeg4_vaapi"
    "--enable-hwaccel=h263_vaapi"
    "--enable-hwaccel=vp8_vaapi"
    "--enable-hwaccel=mjpeg_vaapi"
    "--enable-hwaccel=hevc_vaapi"
    "--enable-encoder=h264_vaapi"
    "--enable-encoder=vp8_vaapi"
    "--enable-encoder=mjpeg_vaapi"
    "--enable-encoder=hevc_vaapi"))

;; ffnvcodec is not supported on ARM then we enable it here for i386 and
;; x86_64 architectures.
(define %ffmpeg-linux-x86-configure-flags
  '("--arch=x86"
    "--enable-cuvid"
    "--enable-ffnvcodec"
    "--enable-nvdec"
    "--enable-nvenc"
    "--enable-hwaccel=h264_nvdec"
    "--enable-hwaccel=hevc_nvdec"
    "--enable-hwaccel=vp8_nvdec"
    "--enable-hwaccel=mjpeg_nvdec"
    "--enable-encoder=h264_nvenc"
    "--enable-encoder=hevc_nvenc"))

;; This procedure composes the configure flags list for ffmpeg-jami.
(define (ffmpeg-compose-configure-flags)
  (define (system=? s)
    (string-prefix? s (%current-system)))

  `(,@%ffmpeg-default-configure-flags
    ,@(if (string-contains (%current-system) "linux")
          (if (or (system=? "i686")
                  (system=? "x86_64"))
              (append %ffmpeg-linux-configure-flags
                      %ffmpeg-linux-x86-configure-flags)
              %ffmpeg-linux-configure-flags)
          '())))

(define-public ffmpeg-jami
  (package/inherit ffmpeg
    (name "ffmpeg-jami")
    (native-inputs
     `(("sfl-patches" ,%sfl-patches)
       ("libiconv" ,libiconv)
       ,@(package-native-inputs ffmpeg)))
    (supported-systems '("x86_64-linux" "i686-linux"
                         "aarch64-linux" "armhf-linux"))
    (arguments
     (append
      '(#:tests? #f)
      (substitute-keyword-arguments (package-arguments ffmpeg)
        ((#:configure-flags '())
         (ffmpeg-compose-configure-flags))
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'unpack 'make-git-checkout-writable
              (lambda _
                (for-each make-file-writable (find-files "."))))
            (add-after 'unpack 'apply-patches
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((jami-apply-dependency-patches
                       ,jami-apply-dependency-patches))
                  ;; These patches come from:
                  ;; "ring-project/daemon/contrib/src/ffmpeg/rules.mak".
                  (jami-apply-dependency-patches
                   #:inputs inputs
                   #:dep-name "ffmpeg"
                   #:patches
                   '("remove-mjpeg-log"
                     "change-RTCP-ratio"
                     "rtp_ext_abs_send_time"
                     "libopusdec-enable-FEC"
                     "libopusenc-enable-FEC"))))))))))))

(define-public libring
  (package
    (name "libring")
    (version %jami-version)
    (source %jami-sources)
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("asio" ,asio)
       ("dbus-c++" ,dbus-c++)
       ("eudev" ,eudev)
       ("ffmpeg" ,ffmpeg-jami)
       ("jack" ,jack-1)
       ("jsoncpp" ,jsoncpp)
       ("libarchive" ,libarchive)
       ("libgit2" ,libgit2)
       ("libnatpmp" ,libnatpmp)
       ("libsecp256k1" ,libsecp256k1)
       ("libupnp" ,libupnp)
       ("opendht" ,opendht)
       ("openssl" ,openssl)
       ("pjproject" ,pjproject-jami)
       ("pulseaudio" ,pulseaudio)
       ("speex" ,speex)
       ("speexdsp" ,speexdsp)
       ("webrtc-audio-processing" ,webrtc-audio-processing)
       ("yaml-cpp" ,yaml-cpp)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gcc" ,gcc-8)                   ;charconv requires GCC 8.1+
       ("libtool" ,libtool)
       ("perl" ,perl)                   ;to generate manpages with pod2man
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (arguments
     `(#:tests? #f         ; The tests fail to compile due to missing headers.
       #:make-flags '("V=1")            ;build verbosely
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "daemon")))
         (add-before 'build 'add-lib-dir
           (lambda _
             (mkdir-p "src/lib"))))))
    (synopsis "Jami core library and daemon")
    (description "This package provides a library and daemon implementing the
Jami core functionality.  Jami is a secure and distributed voice, video and
chat communication platform that requires no centralized server and leaves the
power of privacy in the hands of the user.  It supports the SIP and IAX
protocols, as well as decentralized calling using P2P-DHT.")
    (home-page "https://jami.net/")
    (license license:gpl3+)))

(define-public libringclient
  (package
    (name "libringclient")
    (version %jami-version)
    (source %jami-sources)
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (inputs
     `(("libring" ,libring)
       ("network-manager" ,network-manager)))
    (propagated-inputs
     `(("qtbase" ,qtbase)))     ; Qt is included in several installed headers.
    (arguments
     `(#:tests? #f                      ; There is no testsuite.
       #:configure-flags
       (let ((libring (assoc-ref %build-inputs "libring")))
         (list (string-append "-DRING_XML_INTERFACES_DIR="
                              libring "/share/dbus-1/interfaces")
               (string-append "-DRING_BUILD_DIR=" libring "/include")
               ;; Use LIBWRAP, which removes the requirement on DBus.  Qt
               ;; links with the dbus library in Guix, which expects to find
               ;; its configuration under /etc rather than /usr/share/dbus-1,
               ;; which is perhaps the reason the auto-launching of dring
               ;; doesn't work on foreign distributions.

               ;; FIXME: Disabled for now, as it causes a segfault when
               ;; attempting video calls (see:
               ;; https://git.jami.net/savoirfairelinux/ring-lrc/-/issues/466).
               "-DENABLE_LIBWRAP=false"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "lrc"))))))
    (synopsis "Jami client library")
    (description "This package provides a library common to all Jami clients.
Jami is a secure and distributed voice, video and chat communication platform
that requires no centralized server and leaves the power of privacy in the
hands of the user.  It supports the SIP and IAX protocols, as well as
decentralized calling using P2P-DHT.")
    (home-page "https://jami.net")
    (license license:gpl3+)))

(define-public jami-gnome
  (package
    (name "jami-gnome")
    (version %jami-version)
    (source %jami-sources)
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (inputs
     `(("clutter" ,clutter)
       ("clutter-gtk" ,clutter-gtk)
       ("gtk+" ,gtk+)
       ("libcanberra" ,libcanberra)
       ("libappindicator" ,libappindicator)
       ("libnotify" ,libnotify)
       ("libringclient" ,libringclient)
       ("network-manager" ,network-manager)
       ("qrencode" ,qrencode)
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")))       ;for glib-compile-resources
    (propagated-inputs
     `(("libring" ,libring) ; Contains 'dring', the daemon, which is
                            ; automatically started by DBus.
       ("adwaita-icon-theme" ,adwaita-icon-theme)))
    (arguments
     `(#:tests? #f                      ; There is no testsuite.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "client-gnome"))))))
    (synopsis "Jami client for GNOME")
    (description "This package provides a Jami client for the GNOME desktop.
Jami is a secure and distributed voice, video and chat communication platform
that requires no centralized server and leaves the power of privacy in the
hands of the user.  It supports the SIP and IAX protocols, as well as
decentralized calling using P2P-DHT.")
    (home-page "https://jami.net")
    (license license:gpl3+)))

;;; Keep this until the Qt client matures enough to become the
;;; main 'jami' client.
(define-public jami
  (deprecated-package "jami" jami-gnome))

(define-public jami-qt
  (package
    (name "jami-qt")                    ;to be renamed 'jami' at some point
    (version %jami-version)
    ;; The Qt client code is not yet part of the release tarball; fetch it
    ;; from git for now.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.jami.net/savoirfairelinux/jami-client-qt.git")
                    (commit "ae21c17da5e8f730ae3895ccbc4da8047e3be1eb")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rf3lpk8c4qc12pi6pn4rdp7i8b83xv64yqr0q47rzv9s518qyjp"))))
    (build-system qt-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:tests? #f                      ;no test suite
       ;; TODO: Uncomment after switching back to the tarball source.
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;     (add-after 'unpack 'change-directory
       ;;       (lambda _
       ;;         (chdir "client-qt"))))
       ))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("doxygen" ,doxygen)
       ("graphviz" ,graphviz)))
    (inputs
     `(("libringclient" ,libringclient)
       ("network-manager" ,network-manager)
       ("qrencode" ,qrencode)
       ("qtsvg" ,qtsvg)
       ("qtwebengine" ,qtwebengine)
       ("qtwebchannel" ,qtwebchannel)
       ("qtmultimedia" ,qtmultimedia)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtquickcontrols2" ,qtquickcontrols2)))
    (propagated-inputs
     `(("libring" ,libring)))           ;for dring
    (home-page "https://jami.net")
    (synopsis "Qt Jami client")
    (description "This package provides the Jami Qt client.  Jami is a secure
and distributed voice, video and chat communication platform that requires no
centralized server and leaves the power of privacy in the hands of the user.
It supports the SIP and IAX protocols, as well as decentralized calling using
P2P-DHT.")
    (license license:gpl3+)))
