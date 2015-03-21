;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
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
                #:select (gpl2 gpl2+ gpl3+ lgpl2.1+ bsd-3 public-domain
                               fsf-free isc))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages doxygen)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnutls)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages yasm)
  #:use-module (gnu packages zip))

(define-public liba52
  (package
    (name "liba52")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    ;; A mirror://sourceforge URI doesn't work, presumably
                    ;; because the SourceForge project is misconfigured.
                    "http://liba52.sourceforge.net/files/a52dec-" version
                    ".tar.gz"))
              (sha256
               (base32
                "0czccp4fcpf2ykp16xcrzdfmnircz1ynhls334q374xknd5747d2"))))
    (build-system gnu-build-system)
    (home-page "http://liba52.sourceforge.net/")
    (synopsis "ATSC A/52 stream decoder")
    (description "liba52 is a library for decoding ATSC A/52 streams.  The
A/52 standard is used in a variety of applications, including digital
television and DVD.  It is also known as AC-3.")
    (license gpl2+)))

(define-public libass
  (package
    (name "libass")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/libass/libass/releases/download/"
                    version "/libass-" version ".tar.xz"))
              (sha256
               (base32
                "1mwj2nk9g6cq6f8m1hf0ijg1299rghhy9naahqq43sc2whblb1l7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("yasm" ,yasm)))
    (propagated-inputs
     `(("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("fontconfig" ,fontconfig)
       ("harfbuzz" ,harfbuzz)
       ("enca" ,enca)))
    (home-page "https://github.com/libass/libass")
    (synopsis "Subtitle rendering library for the ASS/SSA format")
    (description "libass is a subtitle rendering library for the
ASS/SSA (Advanced Substation Alpha/SubStation Alpha) subtitle format.")
    (license isc)))

(define-public libcaca
  (package
    (name "libcaca")
    (version "0.99.beta19")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://caca.zoy.org/files/libcaca/libcaca-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1x3j6yfyxl52adgnabycr0n38j9hx2j74la0hz0n8cnh9ry4d2qj"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freeglut" ,freeglut)
       ("ftgl" ,ftgl)
       ("libx11" ,libx11)
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ("zlib" ,zlib)))
    (home-page "http://caca.zoy.org/wiki/libcaca")
    (synopsis "Colour ASCII-art library")
    (description "libcaca is a graphics library that outputs text instead of
pixels, so that it can work on older video cards or text terminals.  It
supports Unicode, 2048 colors, dithering of color images, and advanced text
canvas operations.")
    (license (fsf-free "file://COPYING")))) ;WTFPL version 2

(define-public libdca
  (package
    (name "libdca")
    (version "0.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.videolan.org/pub/videolan/libdca/"
                    version "/libdca-" version ".tar.bz2"))
              (sha256
               (base32
                "0hh6a7l8vvccsd5i1fkv9av2gzv9fy8m0b8jpsn5p6hh4bh2586v"))))
    (build-system gnu-build-system)
    (home-page "http://www.videolan.org/developers/libdca.html")
    (synopsis "DTS Coherent Acoustics decoder")
    (description "libdca is a library for decoding DTS Coherent Acoustics
streams.")
    (license gpl2+)))

(define-public libdv
  (package
    (name "libdv")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/libdv/libdv-" version ".tar.gz"))
              (sha256
               (base32
                "1fl96f2xh2slkv1i1ix7kqk576a0ak1d33cylm0mbhm96d0761d3"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libxv" ,libxv)))
    (home-page "http://libdv.sourceforge.net/")
    (synopsis "DV video (IEC 61834 and SMPTE 314M) codec")
    (description "The Quasar DV codec (libdv) is a software codec for DV
video, the encoding format used by most digital camcorders, typically those
that support the IEEE 1394 (a.k.a. FireWire or i.Link) interface.  Libdv was
developed according to the official standards for DV video: IEC 61834 and
SMPTE 314M.")
    (license lgpl2.1+)))

(define-public libva
  (package
    (name "libva")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.freedesktop.org/software/vaapi/releases/libva/libva-"
             version".tar.bz2"))
       (sha256
        (base32 "01d01mm9fgpwzqycmjjcj3in3vvzcibi3f64icsw2sksmmgb4495"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libdrm" ,libdrm)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("mesa" ,mesa)))
    (home-page "http://www.freedesktop.org/wiki/Software/vaapi/")
    (synopsis "Video acceleration library")
    (description "The main motivation for VA-API (Video Acceleration API) is
to enable hardware accelerated video decode/encode at various
entry-points (VLD, IDCT, Motion Compensation etc.) for prevailing coding
standards (MPEG-2, MPEG-4 ASP/H.263, MPEG-4 AVC/H.264, and VC-1/VMW3).")
    (license expat)))

(define-public ffmpeg
  (package
    (name "ffmpeg")
    (version "2.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "14a7zp8pa1rvw6nr9l2rf57xr004n5kwkhn5lglybjnn1p68xhr3"))))
    (build-system gnu-build-system)
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("opus" ,opus)
       ("ladspa" ,ladspa)
       ("lame" ,lame)
       ("libass" ,libass)
       ("libbluray" ,libbluray)
       ("libcaca" ,libcaca)
       ("libcdio-paranoia" ,libcdio-paranoia)
       ("libquvi" ,libquvi)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("openal" ,openal)
       ("patchelf" ,patchelf)
       ("pulseaudio" ,pulseaudio)
       ("soxr" ,soxr)
       ("speex" ,speex)
       ("twolame" ,twolame)
       ("xvid" ,xvid)
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
               ;; FIXME: only needed for ffmpeg-2.2.13, but easier to add
               ;; globally; drop as soon as ffmpeg-2.2.13 is dropped
              (setenv "LDFLAGS" "-ldl")
;; possible additional inputs:
;;   --enable-avisynth        enable reading of AviSynth script files [no]
;;   --enable-frei0r          enable frei0r video filtering
;;   --enable-libaacplus      enable AAC+ encoding via libaacplus [no]
;;   --enable-libcelt         enable CELT decoding via libcelt [no]
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
;;   --enable-libnut          enable NUT (de)muxing via libnut,
;;                            native (de)muxer exists [no]
;;   --enable-libopencore-amrnb enable AMR-NB de/encoding via libopencore-amrnb [no]
;;   --enable-libopencore-amrwb enable AMR-WB decoding via libopencore-amrwb [no]
;;   --enable-libopencv       enable video filtering via libopencv [no]
;;   --enable-libopenjpeg     enable JPEG 2000 de/encoding via OpenJPEG [no]
;;   --enable-librtmp         enable RTMP[E] support via librtmp [no]
;;   --enable-libschroedinger enable Dirac de/encoding via libschroedinger [no]
;;   --enable-libshine        enable fixed-point MP3 encoding via libshine [no]
;;   --enable-libssh          enable SFTP protocol via libssh [no]
;;                            (libssh2 does not work)
;;   --enable-libstagefright-h264  enable H.264 decoding via libstagefright [no]
;;   --enable-libutvideo      enable Ut Video encoding and decoding via libutvideo [no]
;;   --enable-libv4l2         enable libv4l2/v4l-utils [no]
;;   --enable-libvidstab      enable video stabilization using vid.stab [no]
;;   --enable-libvo-aacenc    enable AAC encoding via libvo-aacenc [no]
;;   --enable-libvo-amrwbenc  enable AMR-WB encoding via libvo-amrwbenc [no]
;;   --enable-libwavpack      enable wavpack encoding via libwavpack [no]
;;   --enable-libx264         enable H.264 encoding via x264 [no]
;;   --enable-libxavs         enable AVS encoding via xavs [no]
;;   --enable-libzmq          enable message passing via libzmq [no]
;;   --enable-libzvbi         enable teletext support via libzvbi [no]
;;   --enable-opencl          enable OpenCL code
;;   --enable-x11grab         enable X11 grabbing [no]
              (zero? (system*
                      "./configure"
                      (string-append "--prefix=" out)
                      "--enable-avresample"
                      "--enable-gpl" ; enable optional gpl licensed parts
                      "--enable-shared"
                      "--enable-fontconfig"
                      ;; "--enable-gnutls" ; causes test failures
                      "--enable-ladspa"
                      "--enable-libass"
                      "--enable-libbluray"
                      "--enable-libcaca"
                      "--enable-libcdio"
                      "--enable-libfreetype"
                      "--enable-libmp3lame"
                      "--enable-libopus"
                      "--enable-libpulse"
                      "--enable-libquvi"
                      "--enable-libsoxr"
                      "--enable-libspeex"
                      "--enable-libtheora"
                      "--enable-libtwolame"
                      "--enable-libvorbis"
                      "--enable-libvpx"
                      "--enable-libxvid"
                      "--enable-openal"

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
    (version "2.2.13")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1vva8ffwxi3rg44byy09qlbiqrrd1h4rmsl5b1mbmvzvwl1lq1l0"))))))

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

;;; This is not version 2; it's a fork literally named "mplayer2".
(define-public mplayer2
  (package
    (name "mplayer2")
    ;; There are no tarballs.  The 2.0 git tag, which is actually the first
    ;; release is from 2011.  The latest commit is from 2013 October, so we
    ;; use that commit.
    (version "201310")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    ;; XXX Change this if mplayer2.org goes up again.
                    (url "http://repo.or.cz/mplayer2.git")
                    (commit "2c378c71a4d9b1df382db9aa787b646628b4e3f9")))
              (sha256
               (base32
                "0s8554sanj6cvnf0h148nsmjgy5v0568nmcza7grpv6fnmddpfam"))
              (file-name (string-append name "-" version "-checkout"))
              ;; Warning: after using this patch, one must pass the -ltheora
              ;; linker flag manually to configure; see below.
              (patches (list (search-patch "mplayer2-theora-fix.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("python" ,python)
       ("python-2" ,python-2)
       ("python-docutils" ,python-docutils)
       ;; ./configure uses which(1) to find rst2man.py.
       ("which" ,which)))
    ;; Missing features: DirectFB, Xss screensaver extensions, VDPAU, MNG,
    ;; libnut, DirectShow TV interface, Radio interfaces of all kinds, vstream
    ;; client, XMSS inputplugin support, joystick, lirc/lircc, and openal.
    ;; OpenAL support is experimental and causes compilation to fail with
    ;; linker errors.
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("faad2" ,faad2)
       ("ffmpeg" ,ffmpeg)
       ("gettext" ,gnu-gettext)
       ("jack" ,jack-2)
       ("ladspa" ,ladspa)
       ("lcms" ,lcms)
       ("liba52" ,liba52)
       ("libass" ,libass)
       ("libbluray" ,libbluray)
       ("libbs2b" ,libbs2b)
       ("libcaca" ,libcaca)
       ("libcdio-paranoia" ,libcdio-paranoia)
       ("libdca" ,libdca)
       ("libdv" ,libdv)
       ("libdvdread" ,libdvdread)
       ("libdvdnav" ,libdvdnav-4)
       ("libjpeg" ,libjpeg)
       ("libmad" ,libmad)
       ("libpng" ,libpng)
       ("libquvi" ,libquvi)
       ("libtheora" ,libtheora)
       ("libungif" ,libungif)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("libxinerama" ,libxinerama)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("mpg123" ,mpg123)
       ("ncurses" ,ncurses)
       ("portaudio" ,portaudio)
       ("pulseaudio" ,pulseaudio)
       ("rsound" ,rsound)
       ("samba" ,samba)
       ("sdl" ,sdl)
       ("speex" ,speex)
       ("xvid" ,xvid)))
    (arguments
     '(#:phases
       (alist-replace
        'configure
        ;; ./configure does not work followed by "SHELL=..." and
        ;; "CONFIG_SHELL=..."; set environment variables instead.
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (setenv "SHELL" (which "bash"))
          (setenv "CONFIG_SHELL" (which "bash"))
          (substitute* "configure"
            (("/usr/X11") (assoc-ref inputs "libx11")))
          (zero?
           (system* "./configure"
                    (string-append "--prefix=" (assoc-ref outputs "out"))
                    "--enable-translation"
                    "--enable-runtime-cpudetection"
                    ;; This is needed in accordance with the theora patch.
                    "--extra-libs=-ltheoradec")))
        (alist-cons-before
         'build 'fix-TOOLS-shebangs
         (lambda _
           (substitute* (find-files "TOOLS" "\\.(sh|pl|py)$")
             (("/usr/bin/env") (which "env"))
             (("/usr/bin/perl") (which "perl"))
             (("/usr/bin/python3") (which "python3"))
             (("/usr/bin/python") (which "python"))))
         (alist-cons-before
          'build 'fix-input-buffer-padding-size
          (lambda _
            (substitute* "libmpdemux/demuxer.h"
              ;; This has to match with FFmpeg's FF_INPUT_BUFFER_PADDING_SIZE,
              ;; which has changed at some point.
              (("(#define MP_INPUT_BUFFER_PADDING_SIZE )[0-9]*" all)
               (string-append all "32"))))
          %standard-phases)))
       ;; No 'check' target.
       #:tests? #f))
    ;; XXX Change this if mplayer2.org goes up again.
    (home-page "http://repo.or.cz/w/mplayer2.git")
    (synopsis "Audio and video player")
    (description "mplayer2 is a general-purpose audio and video player.  It's
a fork of the original MPlayer project, and contains further development in
several areas.")
    ;; See file Copyright.  Most files are gpl2+ or compatible, but talloc.c
    ;; is under lgpl3+, thus the whole project becomes gpl3+.
    (license gpl3+)))

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
                "1aai0h0z1bhp89pxmg4fkrwpmqq24k39fhr15cw6q77m9bccip6k"))
              (patches
               (list (search-patch "libvpx-vp9-out-of-bounds-access.patch")
                     (search-patch "libvpx-fix-ssse3-quantize.patch")
                     (search-patch "libvpx-fix-armhf-link.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (setenv "CONFIG_SHELL" (which "bash"))
                   (let ((out (assoc-ref outputs "out")))
                     (zero? (system* "./configure"
                                     "--enable-shared"
                                     "--as=yasm"
                                     ,@(if (and (not (%current-target-system))
                                                (string-prefix?
                                                 "armhf-"
                                                 (%current-system)))
                                           ;; When building on ARMv7, libvpx
                                           ;; assumes that NEON will be
                                           ;; available.  On Guix, armhf
                                           ;; does not require NEON, so we
                                           ;; build for ARMv6 and -marm (since
                                           ;; no thumb2 on ARMv6) to ensure
                                           ;; compatibility with all ARMv7
                                           ;; cores we support.  Based on
                                           ;; the Debian libvpx package.
                                           '("--target=armv6-linux-gcc"
                                             "--extra-cflags=-marm"
                                             "--enable-small")
                                           '())
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
    (version "2015.01.23.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://youtube-dl.org/downloads/"
                                  version "/youtube-dl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0pvvab9dk1righ3fa79000iz8fzdlcxakscx5sd31730c37j3kj2"))))
    (build-system python-build-system)
    (inputs `(("setuptools" ,python-setuptools)))
    (home-page "http://youtube-dl.org")
    (synopsis "Download videos from YouTube.com and other sites")
    (description
     "youtube-dl is a small command-line program to download videos from
YouTube.com and a few more sites.")
    (license public-domain)))

(define-public libbluray
  (package
    (name "libbluray")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.videolan.org/videolan/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "13dngs4b4cv29f6b825dq14n77mfhvk1kjb42axpq494pfgyp6zp"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("doxygen" ,doxygen)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libxml2" ,libxml2)
       ("perl" ,perl)                   ;for doxygen
       ("texlive" ,texlive)))
    (home-page "http://www.videolan.org/developers/libbluray.html")
    (synopsis "Blu-Ray Disc playback library")
    (description
     "libbluray is a library designed for Blu-Ray Disc playback for media
players, like VLC or MPlayer.")
    (license lgpl2.1+)))

(define-public libdvdread
  (package
    (name "libdvdread")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.videolan.org/videolan/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "052z62l3x8ka5jpf5bi1mzp5p323n1z9rxj74nq5c35a88x1myv6"))))
    (build-system gnu-build-system)
    (home-page "http://dvdnav.mplayerhq.hu/")
    (synopsis "Library for reading video DVDs")
    (description
     "Libdvdread provides a simple foundation for reading DVD video
disks.  It provides the functionality that is required to access many
DVDs.  It parses IFO files, reads NAV-blocks, and performs CSS
authentication and descrambling (if an external libdvdcss library is
installed).")
    (license gpl2+)))

(define-public libdvdnav
  (package
    (name "libdvdnav")
    (version "5.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.videolan.org/videolan/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1ad2lkkiydgwiyqfysra9lkwjv9yqnvcg4hv92hx8qzics1cpcbj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libdvdread" ,libdvdread)))
    (home-page "http://dvdnav.mplayerhq.hu/")
    (synopsis "Library for video DVD navigation features")
    (description
     "Libdvdnav is a library for developers of multimedia
applications.  It allows easy use of sophisticated DVD navigation features
such as DVD menus, multiangle playback and even interactive DVD games.  All
this functionality is provided through a simple API which provides the DVD
playback as a single logical stream of blocks, intermitted by special
dvdnav events to report certain conditions.  The main usage of libdvdnav is
a loop regularly calling a function to get the next block, surrounded by
additional calls to tell the library of user interaction.  The whole
DVD virtual machine and internal playback states are completely
encapsulated.")
    (license gpl2+)))

(define-public libdvdnav-4
  (package
    (inherit libdvdnav)
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "http://download.videolan.org/videolan/libdvdnav/libdvdnav-"
                version ".tar.xz"))
              (sha256
               (base32
                "0wi3gy408c8xj0ism0hckv5jbfh3lg4pmgxv87gbch9jrhp2gjkz"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments
     '(#:phases
       (alist-cons-after
        'unpack 'autoreconf
        (lambda _
          (zero? (system* "autoreconf" "-vif")))
        %standard-phases)))))

(define-public libdvdcss
  (package
    (name "libdvdcss")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.videolan.org/videolan/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "158k9zagmbk5bkbz96l6lwhh7xcgfcnzflkr4vblskhcab6llhbw"))))
    (build-system gnu-build-system)
    (home-page "http://www.videolan.org/developers/libdvdcss.html")
    (synopsis "Library for accessing DVDs as block devices")
    (description
     "libdvdcss is a simple library designed for accessing DVDs like a block
device without having to bother about the decryption.")
    (license gpl2+)))

(define-public srt2vtt
  (package
    (name "srt2vtt")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://dthompson.us/releases/srt2vtt/srt2vtt-"
                    version ".tar.gz"))
              (sha256
               (base32
                "16b377znjm6qlga5yb8aj7b7bcisa1ghcnj2lrb1d30lvxp4liif"))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-2.0)))
    (synopsis "SubRip to WebVTT subtitle converter")
    (description "srt2vtt converts SubRip formatted subtitles to WebVTT format
for use with HTML5 video.")
    (home-page "http://dthompson.us/pages/software/srt2vtt")
    (license gpl3+)))

(define-public avidemux
  (package
    (name "avidemux")
    (version "2.6.8")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/avidemux/avidemux_"
                   version ".tar.gz"))
             (sha256
              (base32
               "10p60wjkzf1bxqcb6i7bx4hbqy3vqg598p3l9lc4v2c9b8iqr682"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    ;; FIXME: Once packaged, add libraries not found during the build.
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("glu" ,glu)
       ("gtk+" ,gtk+)
       ("jack" ,jack-1)
       ("lame" ,lame)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("libxv" ,libxv)
       ("perl" ,perl)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python-wrapper)
       ("qt" ,qt-4)
       ("sdl" ,sdl)
       ("sqlite" ,sqlite)
       ("yasm" ,yasm)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       ;; Make sure files inside the included ffmpeg tarball are
       ;; patch-shebanged.
       (alist-cons-before
        'patch-source-shebangs 'unpack-ffmpeg
        (lambda _
          (with-directory-excursion "avidemux_core/ffmpeg_package"
            (system* "tar" "xf" "ffmpeg-1.2.1.tar.bz2")
            (delete-file "ffmpeg-1.2.1.tar.bz2")))
        (alist-cons-after
         'patch-source-shebangs 'repack-ffmpeg
         (lambda _
           (with-directory-excursion "avidemux_core/ffmpeg_package"
             (substitute* "ffmpeg-1.2.1/configure"
               (("#! /bin/sh") (string-append "#!" (which "bash"))))
             (system* "tar" "cjf" "ffmpeg-1.2.1.tar.bz2" "ffmpeg-1.2.1")
             (delete-file-recursively "ffmpeg-1.2.1")))
         (alist-replace 'configure
          (lambda _
            ;; Copy-paste settings from the cmake build system.
            (setenv "CMAKE_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
            (setenv "CMAKE_INCLUDE_PATH" (getenv "CPATH")))
          (alist-replace 'build
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let*
                ((out (assoc-ref outputs "out"))
                 (lib (string-append out "/lib64"))
                 (top (getcwd))
                 (sdl (assoc-ref inputs "sdl"))
                 (build_component
                   (lambda* (component srcdir #:optional (args '()))
                     (let ((builddir (string-append "build_" component)))
                       (mkdir builddir)
                       (with-directory-excursion builddir
                        (zero? (and
                          (apply system* "cmake"
                                 "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
                                 (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                                 (string-append "-DCMAKE_INSTALL_RPATH=" lib)
                                 (string-append "-DAVIDEMUX_SOURCE_DIR=" top)
                                 (string-append "-DSDL_INCLUDE_DIR="
                                                sdl "/include/SDL")
                                 (string-append "../" srcdir)
                                 args)
                          (system* "make" "-j"
                                   (number->string (parallel-job-count)))
                          (system* "make" "install"))))))))
                (mkdir out)
                (and (build_component "core" "avidemux_core")
                     (build_component "cli" "avidemux/cli")
                     (build_component "qt4" "avidemux/qt4")
                     (build_component "gtk" "avidemux/gtk")
                     (build_component "plugins_common" "avidemux_plugins"
                                     '("-DPLUGIN_UI=COMMON"))
                     (build_component "plugins_cli" "avidemux_plugins"
                                     '("-DPLUGIN_UI=CLI"))
                     (build_component "plugins_qt4" "avidemux_plugins"
                                     '("-DPLUGIN_UI=QT4"))
                     (build_component "plugins_gtk" "avidemux_plugins"
                                     '("-DPLUGIN_UI=GTK"))
                     (build_component "plugins_settings" "avidemux_plugins"
                                     '("-DPLUGIN_UI=SETTINGS")))
                ;; Remove .exe and .dll file.
                (delete-file-recursively
                  (string-append out "/share/ADM6_addons"))))
            (alist-delete 'install
               %standard-phases)))))))
    (home-page "http://fixounet.free.fr/avidemux/")
    (synopsis "Video editor")
    (description "Avidemux is a video editor designed for simple cutting,
filtering and encoding tasks.  It supports many file types, including AVI,
DVD compatible MPEG files, MP4 and ASF, using a variety of codecs.  Tasks
can be automated using projects, job queue and powerful scripting
capabilities.")
    ;; Software with various licenses is included, see License.txt.
    (license gpl2+)))

(define-public avidemux-2.5
  (package (inherit avidemux)
    (version "2.5.6")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/avidemux/avidemux_"
                   version ".tar.gz"))
             (sha256
              (base32
               "12wvxz0n2g85f079d8mdkkp2zm279d34m9v7qgcqndh48cn7znnn"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("gettext" ,gnu-gettext)
       ("gtk+" ,gtk+-2)
       ("jack" ,jack-1)
       ("lame" ,lame)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)
       ("libxv" ,libxv)
       ("perl" ,perl)
       ("pulseaudio" ,pulseaudio)
       ("qt" ,qt-4)
       ("sdl" ,sdl)
       ("yasm" ,yasm)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f
       #:phases
       (alist-cons-before
        'patch-source-shebangs 'unpack-ffmpeg
        (lambda _
          (with-directory-excursion "avidemux/ADM_libraries"
            (system* "tar" "xf" "ffmpeg-0.9.tar.bz2")
            (delete-file "ffmpeg-0.9.tar.bz2")))
        (alist-cons-after
         'patch-source-shebangs 'repack-ffmpeg
         (lambda _
           (with-directory-excursion "avidemux/ADM_libraries"
             (substitute* "ffmpeg-0.9/configure"
               (("#! /bin/sh") (string-append "#!" (which "bash"))))
             (system* "tar" "cjf" "ffmpeg-0.9.tar.bz2" "ffmpeg-0.9")
             (delete-file-recursively "ffmpeg-0.9")))
         (alist-replace 'configure
          (lambda _
            (setenv "CMAKE_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
            (setenv "CMAKE_INCLUDE_PATH" (getenv "CPATH")))
          (alist-replace 'build
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let*
                ((out (assoc-ref outputs "out"))
                 (lib (string-append out "/lib"))
                 (top (getcwd))
                 (sdl (assoc-ref inputs "sdl"))
                 (build_component
                   (lambda* (component srcdir)
                     (let ((builddir (string-append "build_" component)))
                       (mkdir builddir)
                       (with-directory-excursion builddir
                        (zero? (and
                          (system* "cmake"
                                   "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
                                   (string-append "-DCMAKE_INSTALL_PREFIX="
                                                  out)
                                   (string-append "-DCMAKE_INSTALL_RPATH="
                                                  lib)
                                   (string-append "-DAVIDEMUX_SOURCE_DIR="
                                                  top)
                                   (string-append "-DAVIDEMUX_CORECONFIG_DIR="
                                                  top "/build_main/config")
                                   (string-append "-DAVIDEMUX_INSTALL_PREFIX="
                                                  out)
                                   (string-append "-DSDL_INCLUDE_DIR="
                                                  sdl "/include/SDL")
                                   (string-append "../" srcdir))
                          (system* "make" "-j"
                                   (number->string (parallel-job-count)))
                          (system* "make" "install"))))))))
                (mkdir out)
                (and (build_component "main" ".")
                     (build_component "plugins" "plugins"))
                (delete-file-recursively
                  (string-append out "/share/ADM_addons"))))
            (alist-delete 'install
               %standard-phases)))))))))

(define-public vapoursynth
  (package
    (name "vapoursynth")
    (version "26")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/vapoursynth/vapoursynth/archive/R"
                    version ".tar.gz"))
              (sha256
               (base32
                "1qbg5kg0kgrxldd0ckn1s7vy7vx2ig8nqzv6djp38fxccpzw3x9k"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("cython" ,python-cython)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("yasm" ,yasm)))
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("libass" ,libass)
       ("tesseract-ocr" ,tesseract-ocr)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after
          unpack autogen
          (lambda _
            (zero? (system* "sh" "autogen.sh")))))))
    (home-page "http://www.vapoursynth.com/")
    (synopsis "Video processing framework")
    (description "VapourSynth is a C++ library and Python module for video
manipulation.  It aims to be a modern rewrite of Avisynth, supporting
multithreading, generalized colorspaces, per frame properties, and videos with
format changes.")
    ;; As seen from the source files.
    (license lgpl2.1+)))

(define-public xvid
  (package
    (name "xvid")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://downloads.xvid.org/downloads/xvidcore-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0m5g75qvapr7xpywg6a83v5x19kw1nm9l2q48lg7jvvpba0bmqdh"))))
    (build-system gnu-build-system)
    (native-inputs `(("yasm" ,yasm)))
    (arguments
     '(#:phases
       (alist-cons-before
        'configure 'pre-configure
        (lambda _
          (chdir "build/generic")
          (substitute* "configure"
            (("#! /bin/sh") (string-append "#!" (which "sh")))))
        %standard-phases)
       ;; No 'check' target.
       #:tests? #f))
    (home-page "https://www.xvid.com/")
    (synopsis "MPEG-4 Part 2 Advanced Simple Profile video codec")
    (description "Xvid is an MPEG-4 Part 2 Advanced Simple Profile (ASP) video
codec library.  It uses ASP features such as b-frames, global and quarter
pixel motion compensation, lumi masking, trellis quantization, and H.263, MPEG
and custom quantization matrices.")
    (license gpl2+)))
