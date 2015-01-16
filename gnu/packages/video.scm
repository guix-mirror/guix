;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages video)
  #:use-module (ice-9 match)
  #:use-module ((guix licenses)
                #:select (gpl2 gpl2+ bsd-3 public-domain))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages zip)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages yasm))

(define-public ffmpeg
  (package
    (name "ffmpeg")
    (version "2.5.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "06j1cgw9h9ya5z8gpcf9v9zik3l4xz7sr4wshj06kznzz5z3sf4x"))))
    (build-system gnu-build-system)
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("opus" ,opus)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("patchelf" ,patchelf)
       ("speex" ,speex)
       ("zlib", zlib)))
    (native-inputs
     `(("bc" ,bc)
       ("bzip2" ,bzip2)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2) ; scripts use interpreter python2
       ("speex" ,speex)
       ("yasm" ,yasm)))
    (arguments
     `(#:test-target "fate"
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build rpath)
                  (srfi srfi-26))
       #:imported-modules ((guix build gnu-build-system)
                           (guix build utils)
                           (guix build rpath))
       #:phases
         (alist-replace
          'configure
          ;; configure does not work followed by "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key outputs configure-flags #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* "configure"
                (("#! /bin/sh") (string-append "#!" (which "bash"))))
              (setenv "SHELL" (which "bash"))
              (setenv "CONFIG_SHELL" (which "bash"))
;; possible additional inputs:
;;   --enable-avisynth        enable reading of AviSynth script files [no]
;;   --enable-frei0r          enable frei0r video filtering
;;   --enable-ladspa          enable LADSPA audio filtering
;;   --enable-libaacplus      enable AAC+ encoding via libaacplus [no]
;;   --enable-libass          enable libass subtitles rendering [no]
;;   --enable-libbluray       enable BluRay reading using libbluray [no]
;;   --enable-libcaca         enable textual display using libcaca
;;   --enable-libcelt         enable CELT decoding via libcelt [no]
;;   --enable-libcdio         enable audio CD grabbing with libcdio
;;   --enable-libdc1394       enable IIDC-1394 grabbing using libdc1394
;;                            and libraw1394 [no]
;;   --enable-libfaac         enable AAC encoding via libfaac [no]
;;   --enable-libfdk-aac      enable AAC de/encoding via libfdk-aac [no]
;;   --enable-libflite        enable flite (voice synthesis) support via libflite [no]
;;   --enable-libgme          enable Game Music Emu via libgme [no]
;;   --enable-libgsm          enable GSM de/encoding via libgsm [no]
;;   --enable-libiec61883     enable iec61883 via libiec61883 [no]
;;   --enable-libilbc         enable iLBC de/encoding via libilbc [no]
;;   --enable-libmodplug      enable ModPlug via libmodplug [no]
;;   --enable-libmp3lame      enable MP3 encoding via libmp3lame [no]
;;   --enable-libnut          enable NUT (de)muxing via libnut,
;;                            native (de)muxer exists [no]
;;   --enable-libopencore-amrnb enable AMR-NB de/encoding via libopencore-amrnb [no]
;;   --enable-libopencore-amrwb enable AMR-WB decoding via libopencore-amrwb [no]
;;   --enable-libopencv       enable video filtering via libopencv [no]
;;   --enable-libopenjpeg     enable JPEG 2000 de/encoding via OpenJPEG [no]
;;   --enable-libpulse        enable Pulseaudio input via libpulse [no]
;;   --enable-libquvi         enable quvi input via libquvi [no]
;;   --enable-librtmp         enable RTMP[E] support via librtmp [no]
;;   --enable-libschroedinger enable Dirac de/encoding via libschroedinger [no]
;;   --enable-libshine        enable fixed-point MP3 encoding via libshine [no]
;;   --enable-libsoxr         enable Include libsoxr resampling [no]
;;   --enable-libssh          enable SFTP protocol via libssh [no]
;;                            (libssh2 does not work)
;;   --enable-libstagefright-h264  enable H.264 decoding via libstagefright [no]
;;   --enable-libtwolame      enable MP2 encoding via libtwolame [no]
;;   --enable-libutvideo      enable Ut Video encoding and decoding via libutvideo [no]
;;   --enable-libv4l2         enable libv4l2/v4l-utils [no]
;;   --enable-libvidstab      enable video stabilization using vid.stab [no]
;;   --enable-libvo-aacenc    enable AAC encoding via libvo-aacenc [no]
;;   --enable-libvo-amrwbenc  enable AMR-WB encoding via libvo-amrwbenc [no]
;;   --enable-libwavpack      enable wavpack encoding via libwavpack [no]
;;   --enable-libx264         enable H.264 encoding via x264 [no]
;;   --enable-libxavs         enable AVS encoding via xavs [no]
;;   --enable-libxvid         enable Xvid encoding via xvidcore,
;;                            native MPEG-4/Xvid encoder exists [no]
;;   --enable-libzmq          enable message passing via libzmq [no]
;;   --enable-libzvbi         enable teletext support via libzvbi [no]
;;   --enable-openal          enable OpenAL 1.1 capture support [no]
;;   --enable-opencl          enable OpenCL code
;;   --enable-x11grab         enable X11 grabbing [no]
              (zero? (system*
                      "./configure"
                      (string-append "--prefix=" out)
                      "--enable-gpl" ; enable optional gpl licensed parts
                      "--enable-shared"
                      "--enable-fontconfig"
                      ;; "--enable-gnutls" ; causes test failures
                      "--enable-libfreetype"
                      "--enable-libopus"
                      "--enable-libspeex"
                      "--enable-libtheora"
                      "--enable-libvorbis"
                      "--enable-libvpx"

                      "--enable-runtime-cpudetect"

                      ;; Runtime cpu detection is not implemented on
                      ;; MIPS, so we disable some features.
                      "--disable-mips32r2"
                      "--disable-mipsdspr1"
                      "--disable-mipsdspr2"
                      "--disable-mipsfpu"))))
       (alist-cons-after
        'strip 'add-lib-to-runpath
        (lambda* (#:key outputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (lib (string-append out "/lib")))
            ;; Add LIB to the RUNPATH of all the executables and libraries.
            (with-directory-excursion out
              (for-each (cut augment-rpath <> lib)
                        (append (find-files "bin" ".*")
                                (find-files "lib" "\\.so\\..*\\."))))))
          %standard-phases))))
    (home-page "http://www.ffmpeg.org/")
    (synopsis "Audio and video framework")
    (description "FFmpeg is a complete, cross-platform solution to record,
convert and stream audio and video.  It includes the libavcodec
audio/video codec library.")
    (license gpl2+)))

;; We need this older ffmpeg because vlc-2.1.5 doesn't work with ffmpeg-2.4.
(define-public ffmpeg-2.2
  (package (inherit ffmpeg)
    (version "2.2.11")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "06sli7xvihh97ss6a2mkdq4dcj3rg1w8zffrmjfc1hvyjxhc8f2r"))))))

(define-public vlc
  (package
    (name "vlc")
    (version "2.1.5")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://download.videolan.org/pub/videolan/vlc/"
                   version "/vlc-" version ".tar.xz"))
             (sha256
              (base32
               "0whzbn7ahn5maarcwl1yhk9lq10b0q0y9w5pjl9kh3frdjmncrbg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("git" ,git) ; needed for a test
       ("pkg-config" ,pkg-config)))
    ;; FIXME: Add optional inputs once available.
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("avahi" ,avahi)
       ("dbus" ,dbus)
       ("flac" ,flac)
       ("ffmpeg" ,ffmpeg-2.2)     ; FIXME: vlc-2.1.5 won't work with ffmpeg-2.4
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gnutls" ,gnutls)
       ("libcddb" ,libcddb)
       ("libgcrypt" ,libgcrypt)
       ("libkate" ,libkate)
       ("libmad" ,libmad)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libsamplerate" ,libsamplerate)
       ("libssh2" ,libssh2)
       ("libvorbis" ,libvorbis)
       ("libtheora" ,libtheora)
       ("libxext" ,libxext)
       ("libxinerama" ,libxinerama)
       ("libxml2" ,libxml2)
       ("libxpm" ,libxpm)
       ("lua" ,lua-5.1)
       ("mesa" ,mesa)
       ("opus" ,opus)
       ("perl" ,perl)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python-wrapper)
       ("qt" ,qt-4)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("speex" ,speex)
       ("xcb-util-keysyms" ,xcb-util-keysyms)))
    (arguments
     `(#:configure-flags
       `("--disable-a52" ; FIXME: reenable once available
         ,(string-append "LDFLAGS=-Wl,-rpath -Wl,"
                         (assoc-ref %build-inputs "ffmpeg")
                         "/lib")))) ; needed for the tests
    (home-page "https://www.videolan.org/")
    (synopsis "Audio and video framework")
    (description "VLC is a cross-platform multimedia player and framework
that plays most multimedia files as well as DVD, Audio CD, VCD, and various
treaming protocols.")
    (license gpl2+)))

(define-public mplayer
  (package
    (name "mplayer")
    (version "1.1.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.mplayerhq.hu/MPlayer/releases/MPlayer-"
                   version ".tar.xz"))
             (sha256
              (base32
               "0xlcg7rszrwmw29wqr0plsw5d1rq0hb7vjsq7bmmfsly2z1wg3yf"))))
    (build-system gnu-build-system)
    ;; FIXME: Add additional inputs once available.
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cdparanoia" ,cdparanoia)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("lame" ,lame)
       ("libmpg123" ,mpg123)                      ; audio codec for MP3
;;        ("giflib" ,giflib) ; uses QuantizeBuffer, requires version >= 5
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("libxxf86dga" ,libxxf86dga)
       ("libxinerama" ,libxinerama)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("perl" ,perl)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python-wrapper)
       ("sdl" ,sdl)
       ("speex" ,speex)
       ("yasm" ,yasm)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f ; no test target
       #:phases
         (alist-replace
          'configure
          ;; configure does not work followed by "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (libx11 (assoc-ref inputs "libx11")))
              (substitute* "configure"
                (("#! /bin/sh") (string-append "#!" (which "bash"))))
              (setenv "SHELL" (which "bash"))
              (setenv "CONFIG_SHELL" (which "bash"))
              (zero? (system*
                      "./configure"
                      (string-append "--extra-cflags=-I"
                                     libx11 "/include") ; to detect libx11
                      "--disable-tremor-internal" ; forces external libvorbis
                      (string-append "--prefix=" out)
                      ;; Enable runtime cpu detection where supported,
                      ;; and choose a suitable target.
                      ,@(match (or (%current-target-system)
                                   (%current-system))
                          ("x86_64-linux"
                           '("--enable-runtime-cpudetection"
                             "--target=x86_64-linux"))
                          ("i686-linux"
                           '("--enable-runtime-cpudetection"
                             "--target=i686-linux"))
                          ("mips64el-linux"
                           '("--target=mips3-linux")))
                      "--disable-armv5te"
                      "--disable-armv6"
                      "--disable-armv6t2"
                      "--disable-armvfp"
                      "--disable-neon"
                      "--disable-thumb"
                      "--disable-iwmmxt"))))
          %standard-phases)))
    (home-page "http://www.mplayerhq.hu/design7/news.html")
    (synopsis "Audio and video player")
    (description "MPlayer is a movie player.  It plays most MPEG/VOB, AVI,
Ogg/OGM, VIVO, ASF/WMA/WMV, QT/MOV/MP4, RealMedia, Matroska, NUT,
NuppelVideo, FLI, YUV4MPEG, FILM, RoQ, PVA files.  One can watch VideoCD,
SVCD, DVD, 3ivx, DivX 3/4/5, WMV and H.264 movies.")
    (license gpl2)))

(define-public libvpx
  (package
    (name "libvpx")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://webm.googlecode.com/files/libvpx-v"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1aai0h0z1bhp89pxmg4fkrwpmqq24k39fhr15cw6q77m9bccip6k"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (setenv "CONFIG_SHELL" (which "bash"))
                   (let ((out (assoc-ref outputs "out")))
                     (zero? (system* "./configure"
                                     "--enable-shared"
                                     "--as=yasm"
                                     (string-append "--prefix=" out)))))
                 %standard-phases)
       #:tests? #f)) ; no check target
    (native-inputs
     `(("perl" ,perl)
       ("yasm" ,yasm)))
    (synopsis "VP8/VP9 video codec")
    (description "libvpx is a codec for the VP8/VP9 video compression format.")
    (license bsd-3)
    (home-page "http://www.webmproject.org/")))

(define-public youtube-dl
  (package
    (name "youtube-dl")
    (version "2014.12.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://youtube-dl.org/downloads/"
                                  version "/youtube-dl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "09z7v6jxs4a36kyy681mcypcqsxipplnbdy9s3rva1rpp5f74h2z"))))
    (build-system python-build-system)
    (inputs `(("setuptools" ,python-setuptools)))
    (home-page "http://youtube-dl.org")
    (synopsis "Download videos from YouTube.com and other sites")
    (description
     "youtube-dl is a small command-line program to download videos from
YouTube.com and a few more sites.")
    (license public-domain)))
