;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
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
                #:select (gpl2 gpl2+ gpl3+ bsd-3 public-domain))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
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
  #:use-module (gnu packages yasm)
  #:use-module (gnu packages zip))

(define-public ffmpeg
  (package
    (name "ffmpeg")
    (version "2.5.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "11m2hbhdgphjxjp6hk438cxmipqjg5ixbr1kqnn9mbdhq9kc34fc"))))
    (build-system gnu-build-system)
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("opus" ,opus)
       ("lame" ,lame)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("patchelf" ,patchelf)
       ("soxr" ,soxr)
       ("speex" ,speex)
       ("twolame" ,twolame)
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
                      "--enable-libmp3lame"
                      "--enable-libopus"
                      "--enable-libsoxr"
                      "--enable-libspeex"
                      "--enable-libtheora"
                      "--enable-libtwolame"
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
