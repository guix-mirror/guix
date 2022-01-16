;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2019, 2020 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
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
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define %jami-version "20211223.2.37be4c3")

(define %jami-sources
  ;; Return an origin object of the tarball release sources archive of the
  ;; Jami project.
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
        ;; by Jami.
        (delete-file-recursively "daemon/contrib/tarballs")
        ;; Remove the git submodule directories of unused Jami clients.
        (for-each delete-file-recursively '("client-android"
                                            "client-ios"
                                            "client-macosx"
                                            "client-uwp"))))
    (sha256
     (base32
      "12h4a0nj3gh05w64gkywrrb64agrhn6y3q4b9cqnhvr1vdkzlj0h"))))

;; Jami maintains a set of patches for some key dependencies (currently
;; pjproject and ffmpeg) of Jami that haven't yet been integrated upstream.
;; This procedure simplifies the process of applying them.
(define jami-apply-custom-patches
  #~(lambda* (#:key dep-name patches)
      (let ((patches-directory "patches"))
        (mkdir-p patches-directory)
        (invoke "tar" "-xvf" #$%jami-sources
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

(define-public pjproject-jami
  (let ((commit "e1f389d0b905011e0cb62cbdf7a8b37fc1bcde1a")
        (revision "0"))
    (package
      (inherit pjproject)
      (name "pjproject-jami")
      (version (git-version "2.11" revision commit))
      (source (origin
                (inherit (package-source pjproject))
                ;; The Jami development team regularly issues patches to
                ;; pjproject to extend the its functionality and fix bugs;
                ;; they are submitted for inclusion upstream but larger
                ;; patches take time to be reviewed and merged, hence this
                ;; forked repository.
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/savoirfairelinux/pjproject")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0inpmyb6mhrzr0g309d6clkc99lddqdvyf9xajz0igvgp9pvgpza"))))
      (arguments
       (substitute-keyword-arguments (package-arguments pjproject)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'apply-patches
                (lambda _
                  (#$jami-apply-custom-patches
                   #:dep-name "pjproject"
                   #:patches
                   ;; This adds a config_site.h configuration file that sets
                   ;; constants such as PJ_ICE_MAX_CAND that cannot be
                   ;; configured at build time.
                   '("0009-add-config-site")))))))))))

;; The following variables are configure flags used by ffmpeg-jami.  They're
;; from the jami-project/daemon/contrib/src/ffmpeg/rules.mak file.  We try to
;; keep it as close to the official Jami package as possible, to provide all
;; the codecs and extra features that are expected (see:
;; https://review.jami.net/plugins/gitiles/ring-daemon/+/refs/heads/master/contrib/src/ffmpeg/rules.mak)
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

    ;; Enable muxers/demuxers.
    "--disable-demuxers"
    "--disable-muxers"
    "--enable-muxer=rtp"
    "--enable-muxer=g722"
    "--enable-muxer=h263"
    "--enable-muxer=h264"
    "--enable-muxer=hevc"
;    "--enable-muxer=matroska"
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

    ;; Enable parsers.
    "--enable-parser=h263"
    "--enable-parser=h264"
    "--enable-parser=hevc"
    "--enable-parser=mpeg4video"
    "--enable-parser=vp8"
    "--enable-parser=vp9"
    "--enable-parser=opus"

    ;; Encoders/decoders.
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

    ;; Decoders for ringtones and audio streaming.
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

    ;; Encoders/decoders for images.
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

    ;; Filters.
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

;; ffnvcodec is not supported on ARM; enable it only for the i386 and x86_64
;; architectures.
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
    (arguments
     (substitute-keyword-arguments (package-arguments ffmpeg)
       ((#:tests? _ #f)
        ;; The "rtp_ext_abs_send_time" patch causes the 'lavf-mov_rtphint'
        ;; test to fail (see:
        ;; https://git.jami.net/savoirfairelinux/jami-daemon/-/issues/685).
        ;; TODO: Try to disable just this test.
        #f)
       ((#:configure-flags '())
        (ffmpeg-compose-configure-flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'apply-patches
              (lambda _
                ;; These patches come from:
                ;; "jami-project/daemon/contrib/src/ffmpeg/rules.mak".
                (#$jami-apply-custom-patches
                 #:dep-name "ffmpeg"
                 #:patches '("remove-mjpeg-log"
                             "change-RTCP-ratio"
                             "rtp_ext_abs_send_time"
                             "libopusdec-enable-FEC"
                             "libopusenc-enable-FEC"
                             "screen-sharing-x11-fix"))))))))))

(define-public libjami
  (package
    (name "libjami")
    (version %jami-version)
    (source %jami-sources)
    (outputs '("out" "debug"))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; The test suite fails to link when building libjami as a shared library:
      ;; "testAccount_factory.cpp:(.text+0xc52): undefined reference to
      ;; `jami::AccountFactory::AccountFactory()'.
      #:tests? #f
      ;; The agent links the daemon binary with libguile, which enables the
      ;; execution of test plans described in Scheme.  It may be useful in
      ;; user scripts too, until more generalized Scheme bindings are made
      ;; (see: test/agent/README.md).
      ;; FIXME: Fails to link when building libjami as a shared library:
      ;; bindings.cpp:(.text+0x24): undefined reference to `jami::Logger::log
      ;; [...].
      #:configure-flags #~(list "--disable-agent")
      #:make-flags #~(list "V=1")       ;build verbosely
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'change-directory/maybe
            (lambda _
              ;; Allow building from the tarball or a git checkout.
              (false-if-exception (chdir "daemon")))))))
    (inputs
     (list alsa-lib
           asio
           dbus-c++
           eudev
           ffmpeg-jami
           guile-3.0
           jack-1
           jsoncpp
           libarchive
           libgit2
           libnatpmp
           libsecp256k1
           libupnp
           opendht
           openssl
           pjproject-jami
           pulseaudio
           speex
           speexdsp
           webrtc-audio-processing
           yaml-cpp))
    (native-inputs
     (list autoconf
           automake
           cppunit
           libtool
           perl                         ;to generate manpages with pod2man
           pkg-config
           which))
    (synopsis "Jami core library and daemon")
    (description "This package provides a library and daemon implementing the
Jami core functionality.  Jami is a secure and distributed voice, video and
chat communication platform that requires no centralized server and leaves the
power of privacy in the hands of the user.  It supports the SIP and IAX
protocols, as well as decentralized calling using P2P-DHT.")
    (home-page "https://jami.net/")
    (license license:gpl3+)))

;;; Remove when 2023 comes.
(define-public libring
  (deprecated-package "libring" libjami))

;;; Note: this package will eventually be absorbed into jami itself (the Qt
;;; client).
(define-public jami-libclient
  (package
    (name "jami-libclient")
    (version %jami-version)
    (source %jami-sources)
    (build-system cmake-build-system)
    (outputs '("out" "debug"))
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:configure-flags
      #~(list (string-append "-DRING_XML_INTERFACES_DIR="
                             #$(this-package-input "libjami")
                             "/share/dbus-1/interfaces")
              (string-append "-DRING_BUILD_DIR="
                             #$(this-package-input "libjami")
                             "/include")
              ;; Call to the libraries directly instead of going through DBus
              ;; to avoid issues (see: https://issues.guix.gnu.org/48538).
              "-DENABLE_LIBWRAP=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'change-directory/maybe
            (lambda _
              ;; Allow building from the tarball or a git checkout.
              (false-if-exception (chdir "lrc")))))))
    (inputs
     (list libjami network-manager))
    (propagated-inputs
     (list qtbase-5))             ;Qt is included in several installed headers
    (synopsis "Jami client library")
    (description "This package provides a library common to all Jami clients.
Jami is a secure and distributed voice, video and chat communication platform
that requires no centralized server and leaves the power of privacy in the
hands of the user.  It supports the SIP and IAX protocols, as well as
decentralized calling using P2P-DHT.")
    (home-page "https://jami.net")
    (license license:gpl3+)))

;;; Remove when 2023 comes.
(define-public libringclient
  (deprecated-package "libringclient" jami-libclient))

(define-public jami-gnome
  (package
    (name "jami-gnome")
    (version %jami-version)
    (source %jami-sources)
    (outputs '("out" "debug"))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test suite
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build glib-or-gtk-build-system))
       #:modules ((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory/maybe
           (lambda _
             ;; Allow building from the tarball or a git checkout.
             (false-if-exception (chdir "client-gnome"))))
         (add-after 'change-directory/maybe 'fix-webkit-detection
           (lambda _
             (substitute* "CMakeLists.txt"
               (("WEBKIT webkit2gtk-4.0")
                "WEBKIT webkit2gtk-4.1"))))
         (add-after 'change-directory/maybe 'fix-version-string
           (lambda _
             (substitute* "CMakeLists.txt"
               (("^# Set VERSION.*" anchor)
                (string-append anchor
                               "set(PROJECT_VERSION \"" ,version "\")\n")))))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (inputs
     (list clutter
           clutter-gtk
           gtk+
           jami-libclient
           libcanberra
           libappindicator
           libnotify
           network-manager
           qrencode
           sqlite
           webkitgtk))
    (native-inputs
     (list pkg-config
           gettext-minimal
           `(,glib "bin")))             ;for glib-compile-resources
    (synopsis "Jami client for GNOME")
    (description "This package provides a Jami client for the GNOME desktop.
Jami is a secure and distributed voice, video and chat communication platform
that requires no centralized server and leaves the power of privacy in the
hands of the user.  It supports the SIP and IAX protocols, as well as
decentralized calling using P2P-DHT.")
    (home-page "https://jami.net")
    (license license:gpl3+)))

(define-public jami
  (package
    (name "jami")
    (version %jami-version)
    (source (origin
              (inherit %jami-sources)
              (patches (search-patches "jami-fix-crash-on-quit.patch"))))
    (build-system qt-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:tests? #f                      ;no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory/maybe
           (lambda _
             ;; Allow building from the tarball or a git checkout.
             (false-if-exception (chdir "client-qt"))))
         (add-after 'change-directory/maybe 'fix-version-string
           (lambda _
             (substitute* "src/version.h"
               (("VERSION_STRING")
                "BUILD_DATE")           ;to avoid a redefinition error
               (("// clang-format on.*" anchor)
                (string-append "const char VERSION_STRING[] = \""
                               ,version "\";\n"
                               anchor)))))
         (add-after 'change-directory/maybe 'use-desktop-opengl
           ;; TODO: Remove after next release; this is no longer specified in
           ;; the source following the update to Qt 6.
           (lambda _
             (substitute* "src/main.cpp"
               (("Qt::AA_UseOpenGLES")
                "Qt::AA_UseDesktopOpenGL")))))))
    (native-inputs
     (list pkg-config python qttools doxygen graphviz))
    (inputs
     (list jami-libclient
           libnotify
           network-manager
           qrencode
           qtsvg
           qtwebengine
           qtwebchannel
           qtmultimedia
           qtdeclarative
           qtgraphicaleffects
           qtquickcontrols
           qtquickcontrols2))
    (home-page "https://jami.net")
    (synopsis "Qt Jami client")
    (description "This package provides the Jami Qt client.  Jami is a secure
and distributed voice, video and chat communication platform that requires no
centralized server and leaves the power of privacy in the hands of the user.
It supports the SIP and IAX protocols, as well as decentralized calling using
P2P-DHT.")
    (license license:gpl3+)))

;;; Remove when 2023 comes.
(define-public jami-qt
  (deprecated-package "jami-qt" jami))
