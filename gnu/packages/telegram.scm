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
  #:use-module (gnu packages animation)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson))

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
