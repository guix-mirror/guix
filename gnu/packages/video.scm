;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
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
  #:use-module ((guix licenses) #:select (gpl2 gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libpng)
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
    (version "2.2.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "153kfk8rzrfxx930rrk417b2m695dvy47v4hci3nd49iggx9jzz1"))))
    (build-system gnu-build-system)
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("opus" ,opus)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
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
;;   --enable-libvpx          enable VP8 and VP9 de/encoding via libvpx [no]
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
                      ;; drop special machine instructions not supported
                      ;; on all instances of the target
                      ,@(if (string-prefix? "x86_64"
                                            (or (%current-target-system)
                                                (%current-system)))
                            '()
                            '("--disable-amd3dnow"
                              "--disable-amd3dnowext"
                              "--disable-mmx"
                              "--disable-mmxext"
                              "--disable-sse"
                              "--disable-sse2"))
                      "--disable-altivec"
                      "--disable-sse3"
                      "--disable-ssse3"
                      "--disable-sse4"
                      "--disable-sse42"
                      "--disable-avx"
                      "--disable-fma4"
                      "--disable-avx2"
                      "--disable-armv5te"
                      "--disable-armv6"
                      "--disable-armv6t2"
                      "--disable-vfp"
                      "--disable-neon"
                      "--disable-vis"
                      "--disable-mips32r2"
                      "--disable-mipsdspr1"
                      "--disable-mipsdspr2"
                      "--disable-mipsfpu"))))
       (alist-cons-after
        'strip 'add-lib-to-runpath
        (lambda* (#:key outputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (lib (string-append out "/lib")))
            ;; Add LIB to the RUNPATH of all the executables.
            (with-directory-excursion out
              (for-each (cut augment-rpath <> lib)
                        (find-files "bin" ".*")))))
          %standard-phases))))
    (home-page "http://www.ffmpeg.org/")
    (synopsis "Audio and video framework")
    (description "FFmpeg is a complete, cross-platform solution to record,
convert and stream audio and video.  It includes the libavcodec
audio/video codec library.")
    (license gpl2+)))

(define-public vlc
  (package
    (name "vlc")
    (version "2.1.4")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://download.videolan.org/pub/videolan/vlc/"
                   version "/vlc-" version ".tar.xz"))
             (sha256
              (base32
               "1lymhbb2bns73qivdaqanhggjjhyc9fwfgf5ikhng0a74msnqmiy"))))
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
       ("ffmpeg" ,ffmpeg)
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
         "--disable-mmx" ; FIXME: may be enabled on x86_64
         "--disable-sse" ; 1-4, no separate options available
         "--disable-neon"
         "--disable-altivec"
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
                      ;; drop special machine instructions not supported
                      ;; on all instances of the target
                      ,@(if (string-prefix? "x86_64"
                                            (or (%current-target-system)
                                                (%current-system)))
                            '()
                            '("--disable-3dnow"
                              "--disable-3dnowext"
                              "--disable-mmx"
                              "--disable-mmxext"
                              "--disable-sse"
                              "--disable-sse2"))
                      "--disable-ssse3"
                      "--disable-altivec"
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
