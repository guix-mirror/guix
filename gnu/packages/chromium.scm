;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
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

(define-module (gnu packages chromium)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define %preserved-third-party-files
  '("base/third_party/cityhash" ;Expat
    "base/third_party/double_conversion" ;BSD-3
    "base/third_party/dynamic_annotations" ;BSD-2
    "base/third_party/icu" ;Unicode, X11-style
    "base/third_party/superfasthash" ;BSD-3
    "base/third_party/symbolize" ;BSD-3
    "base/third_party/xdg_mime" ;LGPL2.0+ or Academic 2.0
    "base/third_party/xdg_user_dirs" ;Expat
    "chrome/third_party/mozilla_security_manager" ;MPL-1.1/GPL2+/LGPL2.1+
    "courgette/third_party/bsdiff" ;BSD-2, BSD protection license
    "courgette/third_party/divsufsort" ;Expat
    "net/third_party/mozilla_security_manager" ;MPL-1.1/GPL2+/LGPL2.1+
    "net/third_party/nss" ;MPL-2.0
    "net/third_party/quiche" ;BSD-3
    "net/third_party/uri_template" ;ASL2.0
    "third_party/abseil-cpp" ;ASL2.0
    "third_party/adobe/flash/flapper_version.h" ;no license, trivial
    "third_party/angle" ;BSD-3
    "third_party/angle/src/common/third_party/base" ;BSD-3
    "third_party/angle/src/common/third_party/smhasher" ;Public domain
    "third_party/angle/src/common/third_party/xxhash" ;BSD-2
    "third_party/angle/src/third_party/compiler" ;BSD-2
    "third_party/angle/src/third_party/libXNVCtrl" ;Expat
    "third_party/angle/src/third_party/trace_event" ;BSD-3
    "third_party/angle/third_party/vulkan-headers" ;ASL2.0
    "third_party/angle/third_party/vulkan-loader" ;ASL2.0
    "third_party/angle/third_party/vulkan-tools" ;ASL2.0
    "third_party/angle/third_party/vulkan-validation-layers" ;ASL2.0
    "third_party/apple_apsl" ;APSL2.0
    "third_party/axe-core" ;MPL2.0
    "third_party/blink" ;BSD-3, LGPL2+
    "third_party/boringssl" ;OpenSSL/ISC (Google additions are ISC)
    "third_party/boringssl/src/third_party/fiat" ;Expat
    "third_party/breakpad" ;BSD-3
    "third_party/brotli" ;Expat
    "third_party/cacheinvalidation" ;ASL2.0
    "third_party/catapult" ;BSD-3
    "third_party/catapult/common/py_vulcanize/third_party/rcssmin" ;ASL2.0
    "third_party/catapult/common/py_vulcanize/third_party/rjsmin" ;ASL2.0
    "third_party/catapult/third_party/polymer" ;BSD-3
    ;; XXX: This is a minified version of <https://d3js.org/>.
    "third_party/catapult/tracing/third_party/d3" ;BSD-3
    "third_party/catapult/tracing/third_party/gl-matrix" ;Expat
    ;; XXX: Minified version of <https://github.com/Stuk/jszip>.
    "third_party/catapult/tracing/third_party/jszip" ;Expat or GPL3
    "third_party/catapult/tracing/third_party/mannwhitneyu" ;Expat
    "third_party/catapult/tracing/third_party/oboe" ;BSD-2
    ;; XXX: Minified version of <https://github.com/nodeca/pako>.
    "third_party/catapult/tracing/third_party/pako" ;Expat
    "third_party/ced" ;BSD-3
    "third_party/cld_3" ;ASL2.0
    "third_party/closure_compiler" ;ASL2.0
    "third_party/crashpad" ;ASL2.0
    "third_party/crashpad/crashpad/third_party/lss" ;ASL2.0
    "third_party/crashpad/crashpad/third_party/zlib/zlib_crashpad.h" ;Zlib
    "third_party/crc32c" ;BSD-3
    "third_party/cros_system_api" ;BSD-3
    "third_party/dav1d" ;BSD-2
    "third_party/dawn" ;ASL2.0
    "third_party/depot_tools/owners.py" ;BSD-3
    "third_party/devtools-frontend" ;BSD-3
    "third_party/devtools-frontend/src/third_party/axe-core" ;MPL2.0
    "third_party/devtools-frontend/src/third_party/pyjson5" ;ASL2.0
    "third_party/dom_distiller_js" ;BSD-3
    "third_party/emoji-segmenter" ;ASL2.0
    "third_party/flatbuffers" ;ASL2.0
    "third_party/glslang" ;BSD-3, Expat, ASL2.0
    "third_party/google_input_tools" ;ASL2.0
    "third_party/google_input_tools/third_party/closure_library" ;ASL2.0
    "third_party/google_input_tools/third_party/closure_library/third_party/closure" ;Expat
    "third_party/googletest" ;BSD-3
    "third_party/hunspell" ;MPL1.1/GPL2+/LGPL2.1+
    "third_party/iccjpeg" ;IJG
    "third_party/inspector_protocol" ;BSD-3
    "third_party/jinja2" ;BSD-3
    "third_party/jstemplate" ;ASL2.0
    "third_party/khronos" ;Expat, SGI
    "third_party/leveldatabase" ;BSD-3
    "third_party/libXNVCtrl" ;Expat
    "third_party/libaddressinput" ;ASL2.0
    "third_party/libaom" ;BSD-2 or "Alliance for Open Media Patent License 1.0"
    "third_party/libaom/source/libaom/third_party/vector" ;Expat
    "third_party/libaom/source/libaom/third_party/x86inc" ;ISC
    "third_party/libgifcodec" ;MPL1.1/GPL2+/LGPL2.1+, BSD-3, BSD-2
    "third_party/libjingle_xmpp" ;BSD-3
    "third_party/libphonenumber" ;ASL2.0
    "third_party/libsecret" ;LGPL2.1+
    "third_party/libsrtp" ;BSD-3
    "third_party/libsync" ;ASL2.0
    "third_party/libudev" ;LGPL2.1+
    "third_party/libwebm" ;BSD-3
    "third_party/libxml/chromium" ;BSD-3
    "third_party/libyuv" ;BSD-3
    "third_party/lss" ;BSD-3
    "third_party/markupsafe" ;BSD-3
    "third_party/mesa_headers" ;Expat, SGI
    "third_party/metrics_proto" ;BSD-3
    "third_party/modp_b64" ;BSD-3
    "third_party/nasm" ;BSD-2
    "third_party/node" ;Expat
    "third_party/node/node_modules/polymer-bundler/lib/third_party/UglifyJS2" ;BSD-2
    "third_party/one_euro_filter" ;BSD-3
    "third_party/openscreen" ;BSD-3
    "third_party/openscreen/src/third_party/tinycbor" ;Expat
    "third_party/ots" ;BSD-3
    "third_party/pdfium" ;BSD-3
    "third_party/pdfium/third_party/agg23" ;Expat
    "third_party/pdfium/third_party/base" ;BSD-3
    "third_party/pdfium/third_party/bigint" ;Public domain, BSD-3
    "third_party/pdfium/third_party/skia_shared" ;BSD-3
    "third_party/pdfium/third_party/freetype/include/pstables.h" ;FreeType
    "third_party/perfetto" ;ASL2.0
    "third_party/pffft" ;the "FFTPACK" license, similar to BSD-3
    "third_party/ply" ;BSD-3
    "third_party/polymer" ;BSD-3
    "third_party/private-join-and-compute" ;ASL2.0
    "third_party/protobuf" ;BSD-3
    "third_party/protobuf/third_party/six" ;Expat
    "third_party/pyjson5" ;ASL2.0
    "third_party/qcms" ;Expat
    "third_party/rnnoise" ;BSD-3
    "third_party/s2cellid" ;ASL2.0
    "third_party/sfntly" ;ASL2.0
    "third_party/skia" ;BSD-3
    "third_party/skia/include/third_party/skcms" ;BSD-3
    "third_party/skia/third_party/skcms" ;BSD-3
    "third_party/skia/third_party/vulkanmemoryallocator" ;BSD-3, Expat
    "third_party/smhasher" ;Expat, public domain
    "third_party/speech-dispatcher" ;GPL2+
    "third_party/spirv-headers" ;ASL2.0
    "third_party/SPIRV-Tools" ;ASL2.0
    "third_party/sqlite" ;Public domain
    "third_party/ungoogled" ;BSD-3
    "third_party/usb_ids" ;BSD-3
    "third_party/usrsctp" ;BSD-2
    "third_party/wayland/wayland_scanner_wrapper.py" ;BSD-3
    "third_party/wayland-protocols" ;Expat
    "third_party/web-animations-js" ;ASL2.0
    "third_party/webdriver" ;ASL2.0
    "third_party/webrtc" ;BSD-3
    "third_party/webrtc/common_audio/third_party/fft4g" ;Non-copyleft
    "third_party/webrtc/common_audio/third_party/spl_sqrt_floor" ;Public domain
    "third_party/webrtc/modules/third_party/fft" ;Non-copyleft
    "third_party/webrtc/modules/third_party/g711" ;Public domain
    "third_party/webrtc/modules/third_party/g722" ;Public domain
    "third_party/webrtc/rtc_base/third_party/base64" ;Non-copyleft
    "third_party/webrtc/rtc_base/third_party/sigslot" ;Public domain
    "third_party/widevine/cdm/widevine_cdm_version.h" ;BSD-3
    "third_party/widevine/cdm/widevine_cdm_common.h" ;BSD-3
    "third_party/woff2" ;ASL2.0
    "third_party/xdg-utils" ;Expat
    "third_party/yasm/run_yasm.py" ;BSD-2 or BSD-3
    "third_party/zlib/google" ;BSD-3
    "url/third_party/mozilla" ;BSD-3, MPL1.1/GPL2+/LGPL2.1+
    "v8/src/third_party/siphash" ;Public domain
    "v8/src/third_party/utf8-decoder" ;Expat
    "v8/src/third_party/valgrind" ;BSD-4
    "v8/third_party/inspector_protocol" ;BSD-3
    "v8/third_party/v8/builtins")) ;PSFL

(define* (computed-origin-method gexp-promise hash-algo hash
                                 #:optional (name "source")
                                 #:key (system (%current-system))
                                 (guile (default-guile)))
  "Return a derivation that executes the G-expression that results
from forcing GEXP-PROMISE."
  (mlet %store-monad ((guile (package->derivation guile system)))
    (gexp->derivation (or name "computed-origin")
                      (force gexp-promise)
                      #:graft? #f       ;nothing to graft
                      #:system system
                      #:guile-for-build guile)))

(define %chromium-version "80.0.3987.149")
(define %ungoogled-revision "516e2d990a50a4bbeb8c583e56333c2935e2af95")
(define %debian-revision "debian/80.0.3987.116-1")
(define package-revision "0")
(define %package-version (string-append %chromium-version "-"
                                        package-revision "."
                                        (string-take %ungoogled-revision 7)))

(define %chromium-origin
  (origin
    (method url-fetch)
    (uri (string-append "https://commondatastorage.googleapis.com"
                        "/chromium-browser-official/chromium-"
                        %chromium-version ".tar.xz"))
    (sha256
     (base32
      "07icl3hgg1wjkmz88lbpjf6ll4xyi64spki1nmsy6899jgkxvgjh"))))

(define %ungoogled-origin
  (origin
    (method git-fetch)
    (uri (git-reference (url "https://github.com/Eloston/ungoogled-chromium")
                        (commit %ungoogled-revision)))
    (file-name (git-file-name "ungoogled-chromium"
                              (string-take %ungoogled-revision 7)))
    (sha256
     (base32
      "0nm55qq4ahw9haf5g7hmzic4mr2xjgpay7lxps7xjp7s1pda4g0q"))))

(define %debian-origin
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://salsa.debian.org/chromium-team/chromium.git")
          (commit %debian-revision)))
    (file-name (git-file-name "debian-chromium-packaging"
                              (match (string-split %debian-revision #\/)
                                ((_ revision) revision)
                                (_ (string-take %debian-revision 7)))))
    (sha256
     (base32
      "1cc5sp566dd8f2grgr770xwbxgxf58dk1w7q3s8pmv4js5h3pwq8"))))

;; This is a "computed" origin that does the following:
;; *) Runs the Ungoogled scripts on a pristine Chromium tarball.
;; *) Applies Debians Chromium patches, for their unbundling and GCC work.
;; *) Prunes all third_party directories that are not explicitly preserved.
;; *) Adjusts "GN" build files such that system libraries are preferred.
(define ungoogled-chromium-source
  (let ((chromium-source %chromium-origin)
        (ungoogled-source %ungoogled-origin)
        (debian-source %debian-origin))
    (origin
      (method computed-origin-method)
      (file-name (string-append "ungoogled-chromium-" %package-version ".tar.xz"))
      (sha256 #f)
      (uri
       (delay
         (with-imported-modules '((guix build utils))
           #~(begin
               (use-modules (guix build utils)
                            (ice-9 rdelim)
                            (srfi srfi-1)
                            (srfi srfi-26))
               (let ((chromium-dir    (string-append "chromium-" #$%chromium-version))
                     (preserved-files (list #$@%preserved-third-party-files)))

                 (set-path-environment-variable
                  "PATH" '("bin")
                  (list #+(canonical-package patch)
                        #+(canonical-package xz)
                        #+(canonical-package tar)
                        #+python-2
                        #+python))

                 (copy-recursively #+ungoogled-source "/tmp/ungoogled")

                 (with-directory-excursion "/tmp/ungoogled"

                   (format #t "Unpacking chromium tarball...~%")
                   (force-output)
                   (invoke "tar" "xf" #+chromium-source)

                   ;; Ungoogled-Chromium contains a forked subset of the Debian
                   ;; patches.  Disable those, as we apply newer versions later.
                   (substitute* "patches/series"
                     ((".*/debian/.*")
                      ""))

                   (format #t "Ungooglifying...~%")
                   (force-output)
                   (invoke "python3" "utils/prune_binaries.py" chromium-dir
                           "pruning.list")
                   (invoke "python3" "utils/patches.py" "apply"
                           chromium-dir "patches")
                   (invoke "python3" "utils/domain_substitution.py" "apply" "-r"
                           "domain_regex.list" "-f" "domain_substitution.list"
                           "-c" "/tmp/domainscache.tar.gz" chromium-dir)

                   (with-directory-excursion chromium-dir

                     (format #t "Applying Debian patches...~%")
                     (force-output)
                     (let* ((debian  #+debian-source)
                            (patches (string-append debian "/debian/patches"))
                            (series  (string-append patches "/series")))
                       (with-input-from-file series
                         (lambda ()
                           (let loop ((line (read-line)))
                             (unless (eof-object? line)
                               (when (and (> (string-length line) 1)
                                          (not (string-prefix? "#" line))
                                          ;; Skip the Debian-specific ones.
                                          (not (string-prefix? "debianization/" line))
                                          (not (string-prefix? "buster/" line))
                                          (not (any (cute string-suffix? <> line)
                                                    ;; These conflict with Ungoogled.
                                                    '("widevine-buildflag.patch"
                                                      "signin.patch"
                                                      "third-party-cookies.patch"

                                                      ;; Disable workarounds for the
                                                      ;; Chromium "-lite" tarball.  We
                                                      ;; use the "full" version and don't
                                                      ;; need these patches.
                                                      "closure.patch"
                                                      "owners.patch"

                                                      ;; XXX: 'fixes/inspector.patch'
                                                      ;; makes v8 reuse the top-level
                                                      ;; third_party/inspector_protocol
                                                      ;; instead of its own bundled copy,
                                                      ;; but that does not work here for
                                                      ;; some reason.  Ignore that patch
                                                      ;; and those that depend on it.
                                                      "inspector.patch"))))
                                 (invoke "patch" "--force" "-p1" "--input"
                                         (string-append patches "/" line)
                                         "--no-backup-if-mismatch"))
                               (loop (read-line)))))))

                     (format #t "Pruning third party files...~%")
                     (force-output)
                     (apply invoke "python"
                            "build/linux/unbundle/remove_bundled_libraries.py"
                            "--do-remove" preserved-files)

                     (format #t "Replacing GN files...~%")
                     (force-output)
                     (invoke "python3" "build/linux/unbundle/replace_gn_files.py"
                             "--system-libraries" "ffmpeg" "flac" "fontconfig"
                             "freetype" "harfbuzz-ng" "icu" "libdrm" "libevent"
                             "libjpeg" "libpng" "libvpx" "libwebp" "libxml"
                             "libxslt" "openh264" "opus" "re2" "snappy" "yasm"
                             "zlib"))

                   (format #t (string-append "Packing new ungoogled tarball ...~%"))
                   (force-output)
                   (invoke "tar" "cvfa" #$output
                           ;; Avoid non-determinism in the archive.
                           "--mtime=@0"
                           "--owner=root:0"
                           "--group=root:0"
                           "--sort=name"
                           chromium-dir)

                   #t)))))))))

(define opus+custom
  (package/inherit opus
    (name "opus+custom")
    (arguments
     (substitute-keyword-arguments (package-arguments opus)
       ((#:configure-flags flags ''())
        ;; Opus Custom is an optional extension of the Opus
        ;; specification that allows for unsupported frame
        ;; sizes.  Chromium requires that this is enabled.
        `(cons "--enable-custom-modes"
               ,flags))))))

;; Add a custom ld wrapper that supports quoted strings in response files.
;; To be merged with 'ld-wrapper' in a future rebuild cycle.
(define-public ld-wrapper-next
  (let ((orig (car (assoc-ref (%final-inputs) "ld-wrapper"))))
    (package
      (inherit orig)
      (name "ld-wrapper-next")
      (inputs
       `(("wrapper" ,(search-path %load-path
                                  "gnu/packages/ld-wrapper-next.in"))
         ,@(alist-delete "wrapper" (package-inputs orig)))))))

(define-public ungoogled-chromium
  (package
    (name "ungoogled-chromium")
    (version %package-version)
    (synopsis "Graphical web browser")
    (source ungoogled-chromium-source)
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       ;; FIXME: Chromiums RUNPATH lacks entries for some libraries.
       #:validate-runpath? #f
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (ice-9 regex)
                  (srfi srfi-26))
       #:configure-flags
       ;; See tools/gn/docs/cookbook.md and
       ;; https://www.chromium.org/developers/gn-build-configuration
       ;; for usage.  Run "./gn args . --list" in the Release
       ;; directory for an exhaustive list of supported flags.
       ;; (Note: The 'configure' phase will do that for you.)
       (list "is_debug=false"
             "is_cfi=false"
             "use_gold=false"
             "use_lld=false"
             "clang_use_chrome_plugins=false"
             "linux_use_bundled_binutils=false"
             "use_custom_libcxx=false"
             "use_sysroot=false"
             "enable_precompiled_headers=false"
             "goma_dir=\"\""
             "enable_nacl=false"
             "enable_nacl_nonsfi=false"
             "use_allocator=\"none\""
             "use_unofficial_version_number=false"
             "treat_warnings_as_errors=false"
             "use_official_google_api_keys=false"
             "fieldtrial_testing_like_official_build=true"
             "safe_browsing_mode=0"
             "enable_mdns=false"
             "enable_one_click_signin=false"
             "enable_reading_list=false"
             "enable_remoting=false"
             "enable_reporting=false"
             "enable_service_discovery=false"
             "enable_swiftshader=false"
             "enable_vr=false"
             "enable_widevine=false"
             ;; Disable type-checking for the Web UI to avoid a Java dependency.
             "closure_compile=false"

             ;; Define a custom toolchain that simply looks up CC, AR and
             ;; friends from the environment.
             "custom_toolchain=\"//build/toolchain/linux/unbundle:default\""
             "host_toolchain=\"//build/toolchain/linux/unbundle:default\""

             ;; Prefer system libraries.
             "use_system_freetype=true"
             "use_system_harfbuzz=true"
             "use_system_lcms2=true"
             "use_system_libdrm=true"
             "use_system_libjpeg=true"
             "use_system_libopenjpeg2=true"
             "use_system_libpng=true"
             "use_system_zlib=true"
             "use_gnome_keyring=false"  ;deprecated by libsecret
             "use_openh264=true"
             "use_pulseaudio=true"
             "link_pulseaudio=true"

             ;; VA-API acceleration is currently only supported on x86_64-linux.
             ,@(if (string-prefix? "x86_64" (or (%current-target-system)
                                                (%current-system)))
                   '("use_vaapi=true")
                   '())

             ;; Do not artifically restrict formats supported by system ffmpeg.
             "proprietary_codecs=true"
             "ffmpeg_branding=\"Chrome\""

             ;; WebRTC stuff.
             "rtc_use_h264=true"
             ;; Don't use bundled sources.
             "rtc_build_json=false"
             "rtc_build_libevent=false"
             "rtc_build_libvpx=false"
             "rtc_build_opus=false"
             "rtc_build_ssl=false"
             "rtc_build_libsrtp=true"   ;FIXME: fails to find headers
             "rtc_build_usrsctp=true"   ;TODO: package this
             (string-append "rtc_jsoncpp_root=\""
                            (assoc-ref %build-inputs "jsoncpp")
                            "/include/jsoncpp/json\"")
             (string-append "rtc_ssl_root=\""
                            (assoc-ref %build-inputs "openssl")
                            "/include/openssl\""))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-stuff
           (lambda _
             (substitute*
                 '("base/process/launch_posix.cc"
                   "base/third_party/dynamic_annotations/dynamic_annotations.c"
                   "sandbox/linux/seccomp-bpf/sandbox_bpf.cc"
                   "sandbox/linux/services/credentials.cc"
                   "sandbox/linux/services/namespace_utils.cc"
                   "sandbox/linux/services/syscall_wrappers.cc"
                   "sandbox/linux/syscall_broker/broker_host.cc")
               (("include \"base/third_party/valgrind/") "include \"valgrind/"))

             (for-each (lambda (file)
                         (substitute* file
                           ;; Fix opus include path.
                           ;; Do not substitute opus_private.h.
                           (("#include \"opus\\.h\"")
                            "#include \"opus/opus.h\"")
                           (("#include \"opus_custom\\.h\"")
                            "#include \"opus/opus_custom.h\"")
                           (("#include \"opus_defines\\.h\"")
                            "#include \"opus/opus_defines.h\"")
                           (("#include \"opus_multistream\\.h\"")
                            "#include \"opus/opus_multistream.h\"")
                           (("#include \"opus_types\\.h\"")
                            "#include \"opus/opus_types.h\"")))
                       (find-files (string-append "third_party/webrtc/modules"
                                                  "/audio_coding/codecs/opus")))

             (substitute* "chrome/common/chrome_paths.cc"
               (("/usr/share/chromium/extensions")
                ;; TODO: Add ~/.guix-profile.
                "/run/current-system/profile/share/chromium/extensions"))

             ;; Many files try to include ICU headers from "third_party/icu/...".
             ;; Remove the "third_party/" prefix to use system headers instead.
             (substitute* (find-files "chrome" "\\.cc$")
               (("third_party/icu/source/(common|i18n)/")
                ""))

             ;; XXX: Should be unnecessary when use_system_lcms2=true.
             (substitute* "third_party/pdfium/core/fxcodec/icc/iccmodule.h"
               (("include \"third_party/lcms/include/lcms2\\.h\"")
                "include \"lcms2.h\""))

             (substitute*
                 "third_party/breakpad/breakpad/src/common/linux/libcurl_wrapper.h"
               (("include \"third_party/curl") "include \"curl"))

             (substitute* "third_party/webrtc/rtc_base/strings/json.h"
               (("#include \"third_party/jsoncpp/") "#include \"json/"))

             (substitute* "gpu/config/gpu_util.cc"
               (("third_party/vulkan/include/")
                ""))

             (substitute* '("components/viz/common/gpu/vulkan_context_provider.h"
                            "components/viz/common/resources/resource_format_utils.h")
               (("third_party/vulkan/include/") ""))

             (substitute* "third_party/skia/include/gpu/vk/GrVkVulkan.h"
               (("include/third_party/vulkan/") ""))

             ;; Building chromedriver embeds some files using the ZIP
             ;; format which doesn't support timestamps before
             ;; 1980. Therefore, advance the timestamps of the files
             ;; which are included so that building chromedriver
             ;; works.
             (let ((circa-1980 (* 10 366 24 60 60)))
               (for-each (lambda (file)
                           (utime file circa-1980 circa-1980))
                         '("chrome/test/chromedriver/extension/background.js"
                           "chrome/test/chromedriver/extension/manifest.json")))

             #t))
         (add-after 'patch-stuff 'add-absolute-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((cups (assoc-ref inputs "cups"))
                   (nss (assoc-ref inputs "nss"))
                   (mesa (assoc-ref inputs "mesa"))
                   (udev (assoc-ref inputs "udev")))
               (substitute* "printing/cups_config_helper.py"
                 (("cups_config =.*")
                  (string-append "cups_config = '" cups
                                 "/bin/cups-config'\n")))
               (substitute* "crypto/nss_util.cc"
                 (("libnssckbi\\.so")
                  (string-append nss "/lib/nss/libnssckbi.so")))
               (substitute* "device/udev_linux/udev1_loader.cc"
                 (("libudev\\.so\\.1")
                  (string-append udev "/lib/libudev.so.1")))
               (substitute*
                   '("ui/ozone/platform/x11/gl_ozone_glx.cc"
                     "ui/ozone/common/egl_util.cc"
                     "ui/gl/init/gl_initializer_x11.cc"
                     "third_party/angle/src/libANGLE/renderer/gl/glx/FunctionsGLX.cpp")
                 (("libGL\\.so\\.1")
                  (string-append mesa "/lib/libGL.so.1"))
                 (("libEGL\\.so\\.1")
                  (string-append mesa "/lib/libEGL.so.1"))
                 (("libGLESv2\\.so\\.2")
                  (string-append mesa "/lib/libGLESv2.so.2")))
               #t)))
         (add-before 'configure 'prepare-build-environment
           (lambda* (#:key inputs #:allow-other-keys)

             ;; Make sure the right build tools are used.
             (setenv "AR" "ar") (setenv "NM" "nm")
             (setenv "CC" "clang") (setenv "CXX" "clang++")

             ;; Do not optimize away null pointer safety checks.
             (setenv "CXXFLAGS" "-fno-delete-null-pointer-checks")

             ;; TODO: pre-compile instead. Avoids a race condition.
             (setenv "PYTHONDONTWRITEBYTECODE" "1")

             (substitute*
                 ;; From Debians 'system/node.patch'.
                 "third_party/devtools-frontend/src/scripts/devtools_paths.py"
               (("/usr/bin/nodejs") (which "node")))

             ;; XXX: How portable is this.
             (mkdir-p "third_party/node/linux/node-linux-x64")
             (symlink (string-append (assoc-ref inputs "node") "/bin")
                      "third_party/node/linux/node-linux-x64/bin")

             #t))
         (replace 'configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (let ((args (string-join configure-flags " ")))
               ;; Generate ninja build files.
               (invoke "gn" "gen" "out/Release"
                       (string-append "--args=" args))

               ;; Print the full list of supported arguments as well as
               ;; their current status for convenience.
               (format #t "Dumping configure flags...\n")
               (invoke "gn" "args" "out/Release" "--list"))))
         (add-before 'build 'increase-resource-limits
           (lambda _
             ;; XXX: Chromiums linking step requires a lot of simultaneous file
             ;; accesses.  Having a too low ulimit will result in bogus linker
             ;; errors such as "foo.a: error adding symbols: malformed archive".

             ;; Try increasing the soft resource limit of max open files to 2048,
             ;; or equal to the hard limit, whichever is lower.
             (call-with-values (lambda () (getrlimit 'nofile))
               (lambda (soft hard)
                 (when (and soft (< soft 2048))
                   (if hard
                       (setrlimit 'nofile (min hard 2048) hard)
                       (setrlimit 'nofile 2048 #f))
                   (format #t
                           "increased maximum number of open files from ~d to ~d~%"
                           soft (if hard (min hard 2048) 2048)))))
             #t))
         (replace 'build
           (lambda* (#:key (parallel-build? #t) #:allow-other-keys)
             (invoke "ninja" "-C" "out/Release"
                     "-j" (if parallel-build?
                              (number->string (parallel-job-count))
                              "1")
                     "chrome"
                     "chromedriver")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out            (assoc-ref outputs "out"))
                    (bin            (string-append out "/bin"))
                    (exe            (string-append bin "/chromium"))
                    (lib            (string-append out "/lib"))
                    (man            (string-append out "/share/man/man1"))
                    (applications   (string-append out "/share/applications"))
                    (install-regexp (make-regexp "\\.(bin|pak)$"))
                    (locales        (string-append lib "/locales"))
                    (resources      (string-append lib "/resources"))
                    (preferences    (assoc-ref inputs "master-preferences"))
                    (gtk+           (assoc-ref inputs "gtk+"))
                    (sh             (which "sh")))

               (substitute* '("chrome/app/resources/manpage.1.in"
                              "chrome/installer/linux/common/desktop.template")
                 (("@@MENUNAME@@") "Chromium")
                 (("@@PACKAGE@@") "chromium")
                 (("/usr/bin/@@USR_BIN_SYMLINK_NAME@@") exe))

               (mkdir-p man)
               (copy-file "chrome/app/resources/manpage.1.in"
                          (string-append man "/chromium.1"))

               (mkdir-p applications)
               (copy-file "chrome/installer/linux/common/desktop.template"
                          (string-append applications "/chromium.desktop"))

               (mkdir-p lib)
               (copy-file preferences (string-append lib "/master_preferences"))

               (with-directory-excursion "out/Release"
                 (for-each (lambda (file)
                             (install-file file lib))
                           (scandir "." (cut regexp-exec install-regexp <>)))
                 (copy-file "chrome" (string-append lib "/chromium"))

                 (copy-recursively "locales" locales)
                 (copy-recursively "resources" resources)

                 (mkdir-p bin)
                 (symlink "../lib/chromium" exe)
                 (install-file "chromedriver" bin)

                 (wrap-program exe
                   ;; Avoid file manager crash.  See <https://bugs.gnu.org/26593>.
                   `("XDG_DATA_DIRS" ":" prefix (,(string-append gtk+ "/share")))))

               (with-directory-excursion "chrome/app/theme/chromium"
                 (for-each
                  (lambda (size)
                    (let ((icons (string-append out "/share/icons/hicolor/"
                                                size "x" size "/apps")))
                      (mkdir-p icons)
                      (copy-file (string-append "product_logo_" size ".png")
                                 (string-append icons "/chromium.png"))))
                  '("24" "48" "64" "128" "256")))
               #t))))))
    (native-inputs
     `(("bison" ,bison)
       ("clang" ,clang-9)
       ("gn" ,gn)
       ("gperf" ,gperf)
       ("ld-wrapper" ,ld-wrapper-next)
       ("ninja" ,ninja)
       ("node" ,node)
       ("pkg-config" ,pkg-config)
       ("which" ,which)
       ("yasm" ,yasm)

       ;; This file contains defaults for new user profiles.
       ("master-preferences" ,(local-file "aux-files/chromium/master-preferences.json"))

       ("python-beautifulsoup4" ,python2-beautifulsoup4)
       ("python-html5lib" ,python2-html5lib)
       ("python" ,python-2)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("atk" ,atk)
       ("cups" ,cups)
       ("curl" ,curl)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)
       ("expat" ,expat)
       ("flac" ,flac)
       ("ffmpeg" ,ffmpeg)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("harfbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("jsoncpp" ,jsoncpp)
       ("lcms" ,lcms)
       ("libevent" ,libevent)
       ("libffi" ,libffi)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libva" ,libva)
       ("libvpx" ,libvpx)
       ("libwebp" ,libwebp)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxslt" ,libxslt)
       ("libxtst" ,libxtst)
       ("mesa" ,mesa)
       ("minizip" ,minizip)
       ("mit-krb5" ,mit-krb5)
       ("nss" ,nss)
       ("openh264" ,openh264)
       ("openjpeg" ,openjpeg)                          ;PDFium only
       ("openssl" ,openssl)
       ("opus" ,opus+custom)
       ("pango" ,pango)
       ("pciutils" ,pciutils)
       ("pulseaudio" ,pulseaudio)
       ("re2" ,re2)
       ("snappy" ,snappy)
       ("speech-dispatcher" ,speech-dispatcher)
       ("udev" ,eudev)
       ("valgrind" ,valgrind)
       ("vulkan-headers" ,vulkan-headers)))

    ;; Building Chromium takes ... a very long time.  On a single core, a busy
    ;; mid-end x86 system may need more than 24 hours to complete the build.
    (properties '((timeout . 144000)    ;40 hours
                  ;; The linking step may take more than an hour on some hardware.
                  (max-silent-time . 7200)))

    (home-page "https://github.com/Eloston/ungoogled-chromium")
    (description
     "Ungoogled-Chromium is the Chromium web browser, with some functionality
disabled in order to protect the users privacy.")
    ;; Chromium is developed as BSD-3, but bundles a large number of third-party
    ;; components with other licenses.  For full information, see chrome://credits.
    (license (list license:bsd-3
                   license:bsd-2
                   license:expat
                   license:asl2.0
                   license:mpl1.1
                   license:mpl2.0
                   license:public-domain
                   license:isc
                   (license:non-copyleft "chrome://credits"
                                         "See chrome://credits for more information.")
                   license:lgpl2.1+))))

(define-public ungoogled-chromium/wayland
  (package/inherit ungoogled-chromium
    (name "ungoogled-chromium-wayland")
    (inputs
     `(("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ,@(package-inputs ungoogled-chromium)))
    (arguments
     (substitute-keyword-arguments (package-arguments ungoogled-chromium)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'add-ozone-patch
             (lambda _
               ;; Add missing include statement required when using libstdc++,
               ;; Clang and Ozone.  Fixed in M81.
               (substitute* "ui/base/cursor/ozone/bitmap_cursor_factory_ozone.cc"
                 (("#include \"base/logging\\.h" all)
                  (string-append "#include <algorithm>\n" all)))
               #t))))
       ((#:configure-flags flags)
        `(append (list "use_ozone=true"
                       "ozone_platform_wayland=true"
                       "ozone_auto_platforms=false"
                       "ozone_platform=\"wayland\""
                       "use_xkbcommon=true"
                       "use_system_minigbm=true"
                       "use_system_libwayland=true"
                       (string-append "system_wayland_scanner_path=\""
                                      (assoc-ref %build-inputs "wayland")
                                      "/bin/wayland-scanner\""))
                 (delete "use_vaapi=true" ,flags)))))))
