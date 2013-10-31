;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages oggvorbis)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages yasm))

(define-public ffmpeg
  (package
    (name "ffmpeg")
    (version "2.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1pv83nmjgipxwzy5s53834fq0mrqv786zz2w383ki6sfjzyh6rlj"))))
    (build-system gnu-build-system)
    (inputs
     `(("bc" ,bc)
       ("bzip2" ,bzip2)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2) ; scripts use interpreter python2
       ("speex" ,speex)
       ("yasm" ,yasm)
       ("zlib", zlib)))
    (arguments
     `(#:phases
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
;;   --enable-libopus         enable Opus decoding via libopus [no]
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
                      "--enable-libspeex"
                      "--enable-libtheora"
                      "--enable-libvorbis"
                      ;; drop special machine instructions not supported
                      ;; on all instances of the target
                      ,@(if (string-prefix? "x86_64"
                                            (or (%current-target-system)
                                                (%current-system)))
                            '()
                            '("--disable-amd3now"
                              "--disable-amd3nowext"
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
          %standard-phases)))
    (home-page "http://www.ffmpeg.org/")
    (synopsis "Audio and video framework")
    (description "FFmpeg is a complete, cross-platform solution to record,
convert and stream audio and video.  It includes the libavcodec
audio/video codec library.")
    (license gpl2+)))
