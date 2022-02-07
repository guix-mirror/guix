;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 okapi <okapi@firemail.cc>
;;; Copyright © 2018, 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018, 2021 Thorsten Wilms <t_w_@freenet.de>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2019, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019, 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2019 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2019 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2019 Hartmt Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Jonathan Frederickson <jonathan@terracrypt.net>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2021 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Arjan Adriaanse <arjan@adriaan.se>
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

(define-module (gnu packages audio)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet) ; libmicrohttpd
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3) ;taglib
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)  ;libsndfile, libsamplerate
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages linphone)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim) ;xxd
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system waf)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public opensles
  (package
    (name "opensles")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/KhronosGroup/OpenSL-ES-Registry")
         (commit "ea5104bf37bf525c25e6ae2386586048179d0fda")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j5bm7h3ahz66f23i9abwc0y10agfkpksnj6y078x2nichq66h4f"))
       (patches
        (search-patches "opensles-add-license-file.patch"))))
    (build-system copy-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'clean
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/etc"))
               (mkdir-p (string-append out "/include"))
               (mkdir-p (string-append out "/share"))
               (rename-file
                (string-append out "/api/1.1/OpenSLES_IID.c")
                (string-append out "/etc/OpenSLES_IID.c"))
               (rename-file
                (string-append out "/api/1.1/OpenSLES.h")
                (string-append out "/include/OpenSLES.h"))
               (rename-file
                (string-append out "/api/1.1/OpenSLES_Platform.h")
                (string-append out "/include/OpenSLES_Platform.h"))
               (rename-file
                (string-append out "/api/1.1/README.txt")
                (string-append out "/share/README.txt"))
               (rename-file
                (string-append out "/LICENSE.txt")
                (string-append out "/share/LICENSE.txt"))
               (for-each delete-file-recursively
                         (list
                          (string-append out "/api")
                          (string-append out "/specs")))
               (for-each delete-file
                         (list
                          (string-append out "/CODE_OF_CONDUCT.md")
                          (string-append out "/index.php")
                          (string-append out "/README.md"))))
             #t)))))
    (synopsis "Embedded Audio Acceleration")
    (description "OpenSLES is a royalty-free, cross-platform,
hardware-accelerated audio API tuned for embedded systems.  It provides a
standardized, high-performance, low-latency method to access audio
functionality for developers of native applications on embedded mobile
multimedia devices, enabling straightforward cross-platform deployment of
hardware and software audio capabilities, reducing implementation effort, and
promoting the market for advanced audio.")
    (home-page "https://www.khronos.org/opensles/")
    (license (license:non-copyleft "file:///LICENSE.txt"))))

(define-public wildmidi
  (package
    (name "wildmidi")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/Mindwerks/wildmidi")
         (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08fbbsvw6pkwwqarjwcvdp8mq4zn5sgahf025hynwc6rvf4sp167"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; No target
       #:configure-flags
       (list
        "-DWANT_ALSA=ON"
        "-DWANT_OSS=ON"
        "-DWANT_OPENAL=ON")))
    (inputs
     `(("alsa" ,alsa-lib)
       ("openal" ,openal)))
    (synopsis "Software Synthesizer")
    (description "WildMIDI is a simple software midi player which has a core
softsynth library that can be use with other applications.")
    (home-page "https://www.mindwerks.net/projects/wildmidi/")
    (license
     (list
      ;; Library.
      license:lgpl3+
      ;; Player.
      license:gpl3+))))

(define-public webrtc-audio-processing
  (package
    (name "webrtc-audio-processing")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "http://freedesktop.org/software/pulseaudio/"
                       name "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1gsx7k77blfy171b6g3m0k0s0072v6jcawhmx1kjs9w5zlwdkzd0"))))
    (build-system gnu-build-system)
    (arguments
     ;; TODO: Move this to a snippet/patch or remove with the upgrade to 1.0.
     (if (or (target-riscv64?)
             (target-powerpc?))
       (list
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'patch-source
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((patch-file
                        #$(local-file
                           (search-patch
                             "webrtc-audio-processing-big-endian.patch"))))
                   (invoke "patch" "--force" "-p1" "-i" patch-file)
                   (substitute* "webrtc/typedefs.h"
                     (("defined\\(__aarch64__\\)" all)
                      (string-append
                        ;; powerpc-linux
                        "(defined(__PPC__) && __SIZEOF_SIZE_T__ == 4)\n"
                        "#define WEBRTC_ARCH_32_BITS\n"
                        "#define WEBRTC_ARCH_BIG_ENDIAN\n"
                        ;; powerpc64-linux
                        "#elif (defined(__PPC64__) && defined(_BIG_ENDIAN))\n"
                        "#define WEBRTC_ARCH_64_BITS\n"
                        "#define WEBRTC_ARCH_BIG_ENDIAN\n"
                        ;; aarch64-linux
                        "#elif " all
                        ;; riscv64-linux
                        " || (defined(__riscv) && __riscv_xlen == 64)"
                        ;; powerpc64le-linux
                        " || (defined(__PPC64__) && defined(_LITTLE_ENDIAN))"))))))))
       '()))
    (native-inputs
     (if (or (target-riscv64?)
             (target-powerpc?))
       (list
         (local-file (search-patch "webrtc-audio-processing-big-endian.patch"))
         patch)
       '()))
    (synopsis "WebRTC's Audio Processing Library")
    (description "WebRTC-Audio-Processing library based on Google's
implementation of WebRTC.")
    (home-page
     "https://freedesktop.org/software/pulseaudio/webrtc-audio-processing/")
    (license (license:non-copyleft "file:///COPYING"))))

(define-public vo-aacenc
  (package
    (name "vo-aacenc")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/opencore-amr/files/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0dhghm3c8pqrriwwyj5x9i0yf52fmdfijbgqqkvqvwarldvp86p5"))))
    (build-system gnu-build-system)
    (synopsis "VisualOn AAC Encoder")
    (description "VO-AACENC is the VisualOn implementation of Advanced Audio
Coding (AAC) encoder.")
    (home-page "https://sourceforge.net/projects/opencore-amr/")
    (license license:asl2.0)))

(define-public tinyalsa
  (package
    (name "tinyalsa")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/tinyalsa/tinyalsa")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p9khz3bdpdcrnc9p6w522a0ankdchj4nxd3ki41z9401rxmnljq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:make-flags
       (list
        (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (synopsis "ALSA interfacing library")
    (description "TinyALSA is a small library to interface with ALSA in the
Linux kernel.")
    (home-page "https://github.com/tinyalsa/tinyalsa")
    (license (license:non-copyleft "file:///NOTICE"))))

(define-public libgme
  (package
    (name "libgme")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/mpyne/game-music-emu/"
                                  "downloads/game-music-emu-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "07857vdkak306d9s5g6fhmjyxk7vijzjhkmqb15s7ihfxx9lx8xb"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no check target
    (home-page "https://bitbucket.org/mpyne/game-music-emu")
    (synopsis "Video game music file playback library")
    (description
     "Game-music-emu is a collection of video game music file emulators that
support the following formats and systems:
@table @code
@item AY
ZX Spectrum/Asmtrad CPC
@item GBS
Nintendo Game Boy
@item GYM
Sega Genesis/Mega Drive
@item HES
NEC TurboGrafx-16/PC Engine
@item KSS
MSX Home Computer/other Z80 systems (doesn't support FM sound)
@item NSF/NSFE
Nintendo NES/Famicom (with VRC 6, Namco 106, and FME-7 sound)
@item SAP
Atari systems using POKEY sound chip
@item SPC
Super Nintendo/Super Famicom
@item VGM/VGZ
Sega Master System/Mark III, Sega Genesis/Mega Drive, BBC Micro
@end table")
    (license (list license:lgpl2.1+
                   ;; demo and player directories are under the Expat license
                   license:expat))))

(define-public libopenmpt
  (package
    (name "libopenmpt")
    (version "0.5.9")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://download.openmpt.org/archive/libopenmpt/src/"
                       "libopenmpt-" version "+release.autotools.tar.gz"))
       (sha256
        (base32 "0h86p8mnpm98vc4v6jbvrmm02fch7dnn332i26fg3a2s1738m04d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--docdir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'delete-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each delete-file (find-files lib "\\.a$"))
               #t))))))
    (native-inputs
     (list doxygen perl pkg-config))
    (inputs
     `(("alsa" ,alsa-lib)
       ("flac" ,flac)
       ("mpg123" ,mpg123)
       ("portaudio" ,portaudio)
       ("pulseaudio" ,pulseaudio)
       ("sdl2" ,sdl2)
       ("sndfile" ,libsndfile)
       ("vorbis" ,libvorbis)
       ("zlib" ,zlib)))
    (synopsis "Audio tracking library")
    (description "LibOpenMPT is a cross-platform C++ and C module playback
library.  It is based on the player code of the Open ModPlug Tracker project.")
    (home-page "https://openmpt.org/")
    (license (license:non-copyleft "file:///LICENSE"))))

(define-public libofa
  (package
    (name "libofa")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://storage.googleapis.com/"
                       "google-code-archive-downloads/v2/code.google.com/"
                       "musicip-libofa/" name "-" version ".tar.gz"))
       (sha256
        (base32 "184ham039l7lwhfgg0xr2vch2xnw1lwh7sid432mh879adhlc5h2"))
       (patches
        (search-patches
         "libofa-ftbfs-1.diff"
         "libofa-curl.diff"
         "libofa-ftbfs-2.diff"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl expat))
    (propagated-inputs
     (list fftw))
    (synopsis "Open Fingerprint Architecture")
    (description "LibOFA is an audio fingerprint library, created and provided
by MusicIP.")
    (home-page "https://code.google.com/archive/p/musicip-libofa/")
    (license license:gpl2+)))

(define-public faac
  (package
    (name "faac")
    (version "1.30")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/faac/faac-src"
                       "/faac-" version "/faac-1_30.tar.gz"))
       (sha256
        (base32 "1lmj0dib3mjp84jhxc5ddvydkzzhb0gfrdh3ikcidjlcb378ghxd"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (synopsis "Freeware Advanced Audio Coder")
    (description "FAAC is an MPEG-4 and MPEG-2 AAC encoder.")
    (home-page "https://www.audiocoding.com/faac.html")
    (license
     (list
      ;; ISO MPEG-4 reference code.
      license:gpl2+
      ;; Others.
      license:lgpl2.0+))))

(define-public libtimidity
  (package
    (name "libtimidity")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/" name "/files/"
                       name "/" version "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0sif6lxa058b1mg19zwjm8rl2sg8cg0443k4dgi65clz0jy7qi16"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))       ; XXX: LibTiMidity could not be initialised
    (native-inputs
     (list pkg-config))
    (inputs
     (list ao))
    (synopsis "MIDI to WAVE converter library")
    (description "LibTiMidity is a MIDI to WAVE converter library that uses
Gravis Ultrasound-compatible patch files to generate digital audio data from
General MIDI files.")
    (home-page "http://libtimidity.sourceforge.net/")
    (license
     ;; This project is dual-licensed.
     ;; Either of the following licenses can be exercised.
     (list
      license:lgpl2.1+
      license:artistic2.0))))

(define-public vo-amrwbenc
  (package
    (name "vo-amrwbenc")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/opencore-amr/files/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0klx3nkidc6b8aawchpk19n3xlrzgnc046w4gd0rdqphw28v6ljn"))))
    (build-system gnu-build-system)
    (synopsis "Adaptive Multi Rate Codec")
    (description "VO-AMR is a library of VisualOn implementation of
Adaptive Multi Rate Narrowband and Wideband (AMR-NB and AMR-WB) speech codec.")
    (home-page "https://sourceforge.net/projects/opencore-amr/")
    (license license:asl2.0)))

(define-public opencore-amr
  (package
    (name "opencore-amr")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://sourceforge.net/projects/opencore-amr/files/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0hfk9khz3by0119h3jdwgdfd7jgkdbzxnmh1wssvylgnsnwnq01c"))))
    (build-system gnu-build-system)
    (synopsis "Adaptive Multi Rate Codec")
    (description "OpenCore-AMR is a library of OpenCORE Framework
implementation of Adaptive Multi Rate Narrowband and Wideband
(AMR-NB and AMR-WB) speech codec.")
    (home-page "https://sourceforge.net/projects/opencore-amr/")
    (license license:asl2.0)))

(define-public alsa-modular-synth
  (package
    (name "alsa-modular-synth")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/alsamodular/alsamodular"
                                  "/" version "/ams-" version ".tar.bz2"))
              (sha256
               (base32
                "056dn6b9c5nsw2jdww7z1kxrjqqfvxjzxhsd5x9gi4wkwyiv21nz"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib
           ;; We cannot use zita-alsa-pcmi (the successor of clalsadrv) due to
           ;; license incompatibility.
           clalsadrv
           fftw
           jack-1
           ladspa
           liblo
           qtbase-5))
    (native-inputs
     (list pkg-config qttools))
    (home-page "http://alsamodular.sourceforge.net/")
    (synopsis "Realtime modular synthesizer and effect processor")
    (description
     "AlsaModularSynth is a digital implementation of a classical analog
modular synthesizer system.  It uses virtual control voltages to control the
parameters of the modules.  The control voltages which control the frequency
e.g. of the VCO (Voltage Controlled Oscillator) and VCF (Voltage Controlled
Filter) modules follow the convention of 1V / Octave.")
    (license license:gpl2)))

(define-public aubio
  (package
    (name "aubio")
    (version "0.4.9")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://aubio.org/pub/aubio-" version ".tar.bz2"))
             (sha256
              (base32
               "1npks71ljc48w6858l9bq30kaf5nph8z0v61jkfb70xb9np850nl"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:configure-flags
       (list
        (string-append "LDFLAGS=-Wl,-rpath=" (assoc-ref %outputs "out") "/lib")
        "--enable-fftw3f"
        "--enable-jack"
        "--enable-sndfile"
        "--enable-samplerate"
        "--enable-avcodec")
       #:python ,python-2))
    (inputs
     (list jack-1 libsndfile libsamplerate fftwf ffmpeg)) ; for libavcodec
    (native-inputs
     (list pkg-config))
    (home-page "https://aubio.org/")
    (synopsis "Library for audio labelling")
    (description
     "aubio is a tool designed for the extraction of annotations from audio
signals.  Its features include segmenting a sound file before each of its
attacks, performing pitch detection, tapping the beat and producing MIDI
streams from live audio.")
    (license license:gpl3+)))

(define (ardour-rpath-phase major-version)
  `(lambda* (#:key outputs #:allow-other-keys)
     (let ((libdir (string-append (assoc-ref outputs "out")
                                  "/lib/ardour" ,major-version)))
       (substitute* "wscript"
         (("linker_flags = \\[\\]")
          (string-append "linker_flags = [\""
                         "-Wl,-rpath="
                         libdir ":"
                         libdir "/backends" ":"
                         libdir "/engines" ":"
                         libdir "/panners" ":"
                         libdir "/surfaces" ":"
                         libdir "/vamp" "\"]"))))
     #t))

(define-public ardour
  (package
    (name "ardour")
    (version "6.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.ardour.org/ardour/ardour.git")
                    (commit version)))
              (snippet
               ;; Ardour expects this file to exist at build time.  The revision
               ;; is the output of
               ;;    git describe HEAD | sed 's/^[A-Za-z]*+//'
               `(call-with-output-file
                    "libs/ardour/revision.cc"
                  (lambda (port)
                    (format port ,(string-append "#include \"ardour/revision.h\"
namespace ARDOUR { const char* revision = \"" version "\" ; const char* date = \"\"; }"))
                    #t)))
              (sha256
               (base32
                "0vlcbd70y0an881zv87kc3akmaiz4w7whsy3yaiiqqjww35jg1mm"))
              (file-name (string-append name "-" version))))
    (build-system waf-build-system)
    (arguments
     `(#:configure-flags '("--cxx11"          ; required by gtkmm
                           "--optimize"
                           "--no-phone-home"  ; don't contact ardour.org
                           "--freedesktop"    ; build .desktop file
                           "--test")          ; build unit tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-rpath-in-LDFLAGS
          ,(ardour-rpath-phase (version-major version)))
         (add-after 'install 'install-freedesktop-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share"))
                    (ver   ,(version-major version)))
               (for-each
                 (lambda (size)
                   (let ((dir (string-append share "/icons/hicolor/"
                                             size "x" size "/apps")))
                     (mkdir-p dir)
                     (copy-file
                       (string-append "gtk2_ardour/resources/Ardour-icon_"
                                      size "px.png")
                       (string-append dir "/ardour" ver ".png"))))
                 '("16" "22" "32" "48" "256"))
               (install-file (string-append "build/gtk2_ardour/ardour"
                                            ver ".desktop")
                             (string-append share "/applications/"))
               (install-file (string-append "build/gtk2_ardour/ardour"
                                            ver ".appdata.xml")
                             (string-append share "/appdata/")))
             #t)))
       #:test-target "test"))
    (inputs
     (list alsa-lib
           atkmm
           aubio
           boost
           cairomm
           curl
           dbus
           eudev
           fftw
           fftwf
           flac
           glibmm
           gtkmm-2
           jack-1
           libarchive
           libart-lgpl
           libgnomecanvasmm
           liblo
           libogg
           libsamplerate
           libsndfile
           libusb
           libvorbis
           libwebsockets
           libxml2
           lilv
           lrdf
           lv2
           openssl ; Required by libwebsockets.
           pangomm
           python-rdflib
           pulseaudio
           readline
           redland
           rubberband
           serd
           sord
           soundtouch
           sratom
           suil
           taglib
           vamp))
    (native-inputs
     `(("cppunit" ,cppunit)
       ("gettext" ,gettext-minimal)
       ("itstool" ,itstool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (home-page "https://ardour.org")
    (synopsis "Digital audio workstation")
    (description
     "Ardour is a multi-channel digital audio workstation, allowing users to
record, edit, mix and master audio and MIDI projects.  It is targeted at audio
engineers, musicians, soundtrack editors and composers.")
    (license license:gpl2+)))

(define-public audacity
  (package
    (name "audacity")
    (version "3.1.3")
    (source
     (origin
       ;; If built from the release tag, Audacity will describe itself
       ;; as an "Alpha test version" and suggest to users that they use
       ;; the "latest stable released version".
       (method url-fetch)
       (uri (string-append "https://github.com/audacity/audacity/releases/download/"
                           "Audacity-" version "/audacity-" version
                           "-source.tar.gz"))
       (sha256
        (base32
         "189agx11361k9j958s6q5bngnnfx0rwaf0dwbjxy6fwvsb1wv3px"))
       (patches (search-patches "audacity-ffmpeg-fallback.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libraries.
        '(begin
           (for-each
            (lambda (dir)
              (delete-file-recursively (string-append "lib-src/" dir)))
            '("libsoxr" "libvamp" "lv2" "soundtouch" "sqlite" "twolame"
              ;; FIXME: these libraries have not been packaged yet:
              ;; "libnyquist"
              ;; "libscorealign"
              ;; "portburn"
              ;; "portsmf"
              ;; "portmixer"

              ;; FIXME: we have this library, but it differs in that the Slide
              ;; class does not have a member "getInverseStretchedTime".
              ;; "sbsms"
              ))
           #t))))
    (build-system cmake-build-system)
    (inputs
     (list wxwidgets-3.1
           gtk+
           alsa-lib
           jack-1
           expat
           lame
           linux-libre-headers
           flac
           ffmpeg
           libid3tag
           libjpeg-turbo
           libmad
           ;;("libsbsms" ,libsbsms)         ;bundled version is modified
           libsndfile
           soundtouch
           soxr ;replaces libsamplerate
           sqlite
           twolame
           vamp
           libvorbis
           lv2
           lilv ;for lv2
           suil ;for lv2
           portaudio
           portmidi))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)     ;for msgfmt
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("which" ,which)))
    (arguments
     `(#:configure-flags
       (list
        "-Daudacity_conan_enabled=off"
        "-Daudacity_lib_preference=system"
        ;; TODO: enable this flag once we've packaged all dependencies
        ;; "-Daudacity_obey_system_dependencies=on"
        ;; disable crash reports, updates, ..., anything that phones home
        "-Daudacity_has_networking=off")
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
       #:modules
       ((guix build utils)
        (guix build cmake-build-system)
        ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-cmake-rpath
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("\\$ORIGIN/\\.\\./\\$\\{_PKGLIB\\}")
                (string-append (assoc-ref outputs "out") "/lib/audacity"))
               (("CMAKE_BUILD_WITH_INSTALL_RPATH [A-Z]*")
                "CMAKE_BUILD_WITH_INSTALL_RPATH TRUE")
               (("CMAKE_INSTALL_RPATH_USE_LINK_PATH [A-Z]*")
                "CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE"))
             (substitute* "src/CMakeLists.txt"
               (("-Wl,--disable-new-dtags") "-Wl,--enable-new-dtags"))))
         (add-after 'unpack 'comment-out-revision-ident
           (lambda _
             (substitute* "src/CMakeLists.txt"
               (("file\\( TOUCH \".*RevisionIdent\\.h\" \\)" directive)
                (string-append "# " directive)))
             (substitute* "src/AboutDialog.cpp"
               (("(.*RevisionIdent\\.h.*)" include-line)
                (string-append "// " include-line)))))
         (add-after 'unpack 'use-upstream-headers
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("libraries/lib-files/FileNames.cpp")
               (("\"/usr/include/linux/magic.h\"") "<linux/magic.h>"))))
         (add-after 'wrap-program 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))
       ;; The test suite is not "well exercised" according to the developers,
       ;; and fails with various errors.  See
       ;; <http://sourceforge.net/p/audacity/mailman/message/33524292/>.
       #:tests? #f))
    (native-search-paths
     (list (search-path-specification
            (variable "AUDACITY_MODULES_PATH")
            (files '("lib/audacity/modules")))
           (search-path-specification
            (variable "AUDACITY_PATH")
            (files '("share/audacity")))))
    (home-page "https://www.audacityteam.org/")
    (synopsis "Software for recording and editing sounds")
    (description
     "Audacity is a multi-track audio editor designed for recording, playing
and editing digital audio.  It features digital effects and spectrum analysis
tools.")
    (license license:gpl2+)))

(define-public audiofile
  (package
    (name "audiofile")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://audiofile.68k.org/audiofile-" version ".tar.gz"))
       (sha256
        (base32 "0rb927zknk9kmhprd8rdr4azql4gn2dp75a36iazx2xhkbqhvind"))
       (patches
        ;; CVE references according to nixpgs
        (search-patches
         "audiofile-fix-datatypes-in-tests.patch"
         "audiofile-fix-sign-conversion.patch"
         "audiofile-hurd.patch"
         "audiofile-CVE-2015-7747.patch"
         ;; CVE-2017-6829:
         "audiofile-Fix-index-overflow-in-IMA.cpp.patch"
         ;; CVE-2017-6827, CVE-2017-6828, CVE-2017-6832, CVE-2017-6835,
         ;; CVE-2017-6837:
         "audiofile-check-number-of-coefficients.patch"
         ;; CVE-2017-6839:
         "audiofile-overflow-in-MSADPCM.patch"
         ;; CVE-2017-6830, CVE-2017-6834, CVE-2017-6836, CVE-2017-6838:
         "audiofile-multiply-overflow.patch"
         "audiofile-function-signature.patch"
         ;; CVE-2017-6831:
         "audiofile-Fail-on-error-in-parseFormat.patch"
         ;; CVE-2017-6833:
         "audiofile-division-by-zero.patch"
         "audiofile-CVE-2018-13440.patch"
         "audiofile-CVE-2018-17095.patch"))))
    (properties `((lint-hidden-cve . ("CVE-2017-6829"

                                      "CVE-2017-6827" "CVE-2017-6828"
                                      "CVE-2017-6832" "CVE-2017-6835"
                                      "CVE-2017-6837"

                                      "CVE-2017-6839"

                                      "CVE-2017-6830" "CVE-2017-6834"
                                      "CVE-2017-6836" "CVE-2017-6838"

                                      "CVE-2017-6831" "CVE-2017-6833"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib))
    (home-page "https://audiofile.68k.org/")
    (synopsis "Library to handle various audio file formats")
    (description "This is an open-source version of SGI's audiofile library.
It provides a uniform programming interface for processing of audio data to
and from audio files of many common formats.

Currently supported file formats include AIFF/AIFF-C, WAVE, and NeXT/Sun
.snd/.au, BICS, and raw data.  Supported compression formats are currently
G.711 mu-law and A-law.")
    (license license:lgpl2.1+)))

(define-public autotalent
  (package
    (name "autotalent")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://tombaran.info/autotalent-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1n04qm66f14195ly6gsy3ra7v2j7zad5n19d8dwfmh0qs6h9hphh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr/lib64/ladspa")
                (string-append (assoc-ref outputs "out") "/lib/ladspa")))
             #t)))))
    (inputs
     (list ladspa))
    (home-page "http://tombaran.info/autotalent.html")
    (synopsis "Pitch-correction LADSPA audio plugin")
    (description
     "Autotalent is a LADSPA plugin for real-time pitch-correction.  Among its
controls are allowable notes, strength of correction, LFO for vibrato and
formant warp.")
    ;; All code except the FFT routine is licensed under GPLv2+.
    ;; The FFT routine is under BSD-3.
    (license license:gpl2+)))

(define-public azr3
  (package
    (name "azr3")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/ll-plugins/azr3-jack-"
                                  version
                                  ".tar.bz2"))
              (sha256
               (base32
                "18mdw6nc0vgj6k9rsy0x8w64wvzld0frqshrxxbxfj9qi9843vlc"))
              (patches (search-patches "azr3.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags
       (list "LV2PEG=ttl2c"
             (string-append "prefix=" %output)
             (string-append "pkgdatadir=" %output "/share/azr3-jack"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'fix-timestamp
           (lambda _
             (let ((early-1980 315619200)) ; 1980-01-02 UTC
               (utime "azr3.1" early-1980 early-1980))
             #t)))))
    (inputs
     (list gtkmm-2 lvtk jack-1 lash))
    (native-inputs
     (list pkg-config))
    (home-page "http://ll-plugins.nongnu.org/azr3/")
    (synopsis "Tonewheel organ synthesizer")
    (description
     "AZR-3 is a port of the free VST plugin AZR-3.  It is a tonewheel organ
with drawbars, distortion and rotating speakers.  The organ has three
sections, two polyphonic sections with nine drawbars each and one monophonic
bass section with five drawbars.  A standalone JACK application and LV2
plugins are provided.")
    (license license:gpl2)))

(define-public calf
  (package
    (name "calf")
    (version "0.90.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://calf-studio-gear.org/files/calf-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17x4hylgq4dn9qycsdacfxy64f5cv57n2qgkvsdp524gnqzw4az3"))))
    (build-system gnu-build-system)
    (inputs
     (list fluidsynth
           expat
           glib
           gtk+-2
           cairo
           lash
           jack-1
           lv2
           ladspa
           fftw))
    (native-inputs
     (list pkg-config))
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))
    (home-page "http://calf.sourceforge.net/")
    (synopsis "Audio plug-in pack for LV2 and JACK environments")
    (description
     "Calf Studio Gear is an audio plug-in pack for LV2 and JACK environments.
The suite contains lots of effects (delay, modulation, signal processing,
filters, equalizers, dynamics, distortion and mastering effects),
instruments (SF2 player, organ simulator and a monophonic synthesizer) and
tools (analyzer, mono/stereo tools, crossovers).")
    ;; calfjackhost is released under GPLv2+
    ;; The plugins are released under LGPLv2.1+
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public caps-plugins-lv2
  (package
    (name "caps-plugins-lv2")
    (version "0.9.26")
    (source
     (origin
       ;; The Github project hasn't tagged a release.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/moddevices/caps-lv2.git")
             (commit "5d52a0c6e8863c058c2aab2dea9f901a90d96eb9")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0hdl7n3ra5gqgwkdfqkw8dj9gb1cgb76bn1v91w06d2w4lj9s8xa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (find-files "plugins" "Makefile")
               (("/usr/local")(assoc-ref outputs "out")))
             #t)))))
    (inputs
     (list lv2))
    ;; home-page of the original LADSPA version: http://quitte.de/dsp/caps.html
    (home-page "https://github.com/moddevices/caps-lv2")
    (synopsis "LV2 port of the CAPS audio plugin collection")
    (description
     "LV2 port of CAPS, a collection of audio plugins comprising basic virtual
guitar amplification and a small range of classic effects, signal processors and
generators of mostly elementary and occasionally exotic nature.")
    (license license:gpl3+)))

(define-public infamous-plugins
  (package
    (name "infamous-plugins")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ssj71/infamousPlugins")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r72agk5nxf5k0mghcc2j90z43j5d9i7rqjmf49jfyqnd443isip"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-compiler-flags
           (lambda _
             (substitute* (find-files "." "CMakeLists.txt")
               (("-msse2 -mfpmath=sse") ""))
             #t))
         (add-after 'unpack 'fix-build-with-newer-lv2
           (lambda _
             ;; https://github.com/ssj71/infamousPlugins/commit/4c7275b1fa8ea3296446421cbd29ec2df66588c0
             (substitute* (find-files "src" ".*\\.cxx")
               (("_LV2UI_Descriptor") "LV2UI_Descriptor"))
             #t)))))
    (inputs
     (list cairo fftwf lv2 ntk zita-resampler))
    (native-inputs
     (list pkg-config))
    (home-page "https://ssj71.github.io/infamousPlugins")
    (synopsis "LV2 plugins for live use")
    (description
     "The infamous plugins are a collection of LV2 audio plugins for live
performances.  The plugins include a cellular automaton synthesizer, an
envelope follower, distortion effects, tape effects and more.")
    (license license:gpl2+)))

(define-public snapcast
  (package
    (name "snapcast")
    (version "0.24.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/badaix/snapcast")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13yz8alplnqwkcns3mcli01qbyy6l3h62xx0v71ygcrz371l4g9g"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no included tests
    (inputs
     (list boost
           libvorbis
           soxr
           alsa-lib
           avahi
           pulseaudio
           flac
           opus))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/badaix/snapcast")
    (synopsis "Synchronous multiroom audio player")
    (description
     "Snapcast is a multi-room client-server audio player.  Clients are time
synchronized with the server to play synced audio.")
    (license license:gpl3+)))

(define-public swh-plugins
  (package
    (name "swh-plugins")
    (version "0.4.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/swh/ladspa")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c98z2xxz9pgcb4dg99gz8qrylh5cnag0j18a52d88ifsy24isvq"))))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal ;for autopoint
           libtool
           perl
           pkg-config
           which))
    (inputs
     (list fftwf perl-xml-parser))
    (build-system gnu-build-system)
    (home-page "http://plugin.org.uk")
    (synopsis "The SWH Plugins package for the LADSPA plugin system")
    (description "This package provides Steve Harris's LADSPA plugins.")
    (license license:gpl2+)))

(define-public swh-plugins-lv2
  (package
    (name "swh-plugins-lv2")
    (version "1.0.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/swh/lv2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y7nnww864mm4k6ayy2lhcws3wlbhb2gkyjbrwk921fvc18qk9mz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-after 'unpack 'patch-makefile-and-enter-directory
           ;; The default install target doesn't install, but the
           ;; "install-system" target does.
           (lambda _
             (substitute* "Makefile"
               (("install:") "install: install-system"))
             #t)))))
    (inputs
     (list lv2 fftwf))
    (native-inputs
     (list libxslt pkg-config))
    (home-page "http://plugin.org.uk")
    (synopsis "SWH plugins in LV2 format")
    (description
     "Swh-plugins-lv2 is a collection of audio plugins in LV2 format.  Plugin
classes include: dynamics (compressor, limiter), time (delay, chorus,
flanger), ringmodulator, distortion, filters, pitchshift, oscillators,
emulation (valve, tape), bit fiddling (decimator, pointer-cast), etc.")
    (license license:gpl3+)))

(define-public libdjinterop
  (package
    (name "libdjinterop")
    (version "0.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xsco/libdjinterop")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16nrqpr90vb9ggmp9j73m0hspd7pmfdhh0g6iyp8vd7kx7g17qnk"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; crate_test writes a database file to the source tree.
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files ".")))))))
    (native-inputs
     (list boost pkg-config))
    (inputs
     (list sqlite zlib))
    (home-page "https://github.com/xsco/libdjinterop")
    (synopsis "C++ library for access to DJ record libraries")
    (description
     "@code{libdjinterop} is a C++ library that allows access to database
formats used to store information about DJ record libraries.")
    (license license:lgpl3+)))

(define-public tao
  (package
    (name "tao")
    (version "1.0-beta-10May2006")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/taopm/Tao/"
                                  "tao-" version "/"
                                  "tao-" version ".tar.gz"))
              (sha256
               (base32
                "156py3g6mmglldfd0j76bn7n242hdwf49diaprjpj7crp8vgf2pz"))
              (patches
               (search-patches "tao-add-missing-headers.patch"
                               "tao-fix-parser-types.patch"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "configure"
                  (("SHELL=/bin/sh") "")))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("TAO_RELEASE=-beta")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-references
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "user-scripts/tao.in"
               (("taoparse")
                (string-append (assoc-ref outputs "out") "/bin/taoparse"))
               (("grep") (which "grep"))
               (("sed -f \\$distdir/user-scripts/")
                (string-append (which "sed") " -f $distdir/"))
               (("distdir=.*")
                (string-append "distdir="
                               (assoc-ref outputs "out") "/share/tao")))))
         (add-after 'install 'install-extra-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/tao/"))
                    (inc (string-append out "/include/tao/")))
               (mkdir-p share)
               (mkdir-p inc)
               (install-file "user-scripts/error.parse" share)
               (copy-recursively "examples" (string-append share "examples"))
               (for-each (lambda (file) (install-file file inc))
                         (find-files "include" "\\.h"))))))))
    (inputs
     (list audiofile
           libxi
           libxmu
           mesa
           freeglut
           flex
           bison
           sed
           grep))
    (home-page "http://taopm.sourceforge.net/")
    (synopsis "Sound Synthesis with Physical Models")
    (description "Tao is a software package for sound synthesis using physical
models.  It provides a virtual acoustic material constructed from masses and
springs which can be used as the basis for building quite complex virtual
musical instruments.  Tao comes with a synthesis language for creating and
playing instruments and a C++ API for those who would like to use it as an
object library.")
    (license license:gpl2+)))

(define-public tao-synth
  (let ((commit "f3aedd81efbc775574e591081b57ae1c08427064")
        (revision "1"))
    (package
      (name "tao-synth")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lucasw/tao_synth")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jds2l3cb96b02jxd7lmrjjl9s7mylnrvg6fpw0j8c141bk8vyg3"))))
      (build-system cmake-build-system)
      (arguments (list #:tests? #false))  ;there are no tests
      (inputs
       (list glfw freeglut))
      (native-inputs
       (list gcc-7))
      (home-page "https://github.com/lucasw/tao_synth")
      (synopsis "Sound synthesis with physical models")
      (description "Tao is a software package for sound synthesis using physical
models.  It provides a virtual acoustic material constructed from masses and
springs which can be used as the basis for building quite complex virtual
musical instruments.  Tao comes with a synthesis language for creating and
playing instruments and a C++ API for those who would like to use it as an
object library.")
      (license license:lgpl2.0+))))

(define-public csound
  (package
    (name "csound")
    (version "6.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/csound/csound")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sr9knfhbm2m0wpkjq2l5n471vnl51wy4p6j4m95zqybimzb4s2j"))))
    (build-system cmake-build-system)
    (native-inputs
     (list bison flex gettext-minimal zlib))
    (inputs
     (list alsa-lib
           boost
           jack-1
           ladspa
           liblo
           libsndfile
           pulseaudio))
    (home-page "https://csound.com/")
    (synopsis "Sound and music computing system")
    (description
     "Csound is a user-programmable and user-extensible sound processing
language and software synthesizer.")
    (license license:lgpl2.1+)))

(define-public midicomp
  ;; The latest tagged release is 9 years old and there have been
  ;; unreleased fixes, so we take the last commit.
  (let ((commit "70f76963cb0cdb3cbe03ec6e7246b1fb885d3c68")
        (revision "1"))
    (package
      (name "midicomp")
      (version (git-version "0.0.8" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/markc/midicomp")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "12zh247c6v88ssy4l8v7yirh4bl2jcc1ch7f4gdry79a82kai1gf"))))
     (build-system cmake-build-system)
     (arguments
      `(#:tests? #f))  ; no "check" target
      (synopsis "Convert SMF MIDI files to and from plain text")
      (description
       "midicomp can manipulate SMF (Standard MIDI File) files.  It can both
  read and write SMF files in 0 or format 1 and also read and write its own
  plain text format.  This means a SMF file can be turned into easily
  parseable text, edited with any text editor or filtered through any script
  language, and recompiled back into a binary SMF file.")
      (home-page "https://github.com/markc/midicomp")
      (license license:agpl3))))

(define-public clalsadrv
  (package
    (name "clalsadrv")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/clalsadrv-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0bsacx3l9065gk8g4137qmz2ij7s9x06aldvacinzlcslw7bd1kq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile-and-enter-directory
           (lambda _
             (substitute* "libs/Makefile"
               (("/sbin/ldconfig") "true")
               (("^LIBDIR =.*") "LIBDIR = lib\n"))
             (chdir "libs")
             #t))
         (add-after 'install 'install-symlink
           (lambda _
             (symlink "libclalsadrv.so"
                      (string-append (assoc-ref %outputs "out")
                                     "/lib/libclalsadrv.so.2"))
             #t))
         ;; no configure script
         (delete 'configure))))
    (inputs
     (list alsa-lib fftw))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "C++ wrapper around the ALSA API")
    (description
     "clalsadrv is a C++ wrapper around the ALSA API simplifying access to
ALSA PCM devices.")
    (license license:gpl2+)))

(define-public amb-plugins
  (package
    (name "amb-plugins")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/AMB-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0x4blm4visjqj0ndqr0cg776v3b7lvplpc8cgi9n51llhavn0jpl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory-and-tool-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr/lib/ladspa")
                (string-append (assoc-ref outputs "out") "/lib/ladspa"))
               (("/usr/bin/install") (which "install"))
               (("/bin/rm") "#"))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA ambisonics plugins")
    (description
     "The AMB plugins are a set of LADSPA ambisonics plugins, mainly to be
used within Ardour.  Features include: mono and stereo to B-format panning,
horizontal rotator, square, hexagon and cube decoders.")
    (license license:gpl2+)))

(define-public mcp-plugins
  (package
    (name "mcp-plugins")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/MCP-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "06a9r1l85jmg7l1cvc3788mk8ra0xagjfy1rmhw3b80y4n0vlnvc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "Chorus, phaser, and vintage high-pass and low-pass filters")
    (description
     "This package provides various LADSPA plugins.  @code{cs_chorus} and
@code{cs_phaser} provide chorus and phaser effects, respectively;
@code{mvclpf24} provides four implementations of the low-pass filter used in
vintage Moog synthesizers; @code{mvchpf24} is based on the voltage-controlled
high-pass filter by Robert Moog.  The filters attempt to accurately emulate
the non-linear circuit elements of their original analog counterparts.")
    (license license:gpl2+)))

(define-public rev-plugins
  (package
    (name "rev-plugins")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/REV-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1ikpinxm00pkfi259bnkzhsy3miagrjgdihaaf5x4v7zac29j3g7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA reverb plugin")
    (description
     "This package provides a stereo reverb LADSPA plugin based on the
well-known greverb.")
    (license license:gpl2+)))

(define-public fil-plugins
  (package
    (name "fil-plugins")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/FIL-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1scfv9j7jrp50r565haa4rvxn1vk2ss86xssl5qgcr8r45qz42qw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA four-band parametric equalizer plugin")
    (description
     "This package provides a LADSPA plugin for a four-band parametric
equalizer.  Each section has an active/bypass switch, frequency, bandwidth and
gain controls.  There is also a global bypass switch and gain control.

The 2nd order resonant filters are implemented using a Mitra-Regalia style
lattice filter, which is stable even while parameters are being changed.

All switches and controls are internally smoothed, so they can be used @code{live}
without any clicks or zipper noises.  This makes this plugin suitable for use
in systems that allow automation of plugin control ports, such as Ardour, or
for stage use.")
    (license license:gpl2+)))

(define-public ste-plugins
  (package
    (name "ste-plugins")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/STE-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0s3c9w5xihs87cnd1lh9xgj3maabjdyh6bl766qp5lhkg3ax8zy6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA stereo width plugin")
    (description
     "This package provides a LADSPA plugin to manipulate the stereo width of
audio signals.")
    (license license:gpl2+)))

(define-public vco-plugins
  (package
    (name "vco-plugins")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/VCO-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1xzqdg3b07r7zww05y9bb737l9dxvfkv28m3fyak1aazaci3rsgl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out"))
               (("/bin/cp") (which "cp")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA plugin for synthesizer oscillators")
    (description
     "The @code{blvco} LADSPA plugin provides three anti-aliased oscillators:

@enumerate
@item Pulse-VCO, a dirac pulse oscillator with flat amplitude spectrum
@item Saw-VCO, a sawtooth oscillator with 1/F amplitude spectrum
@item Rec-VCO, a square / rectangle oscillator
@end enumerate\n

All oscillators are low-pass filtered to provide waveforms similar to the
output of analog synthesizers such as the Moog Voyager.")
    (license license:gpl2+)))

(define-public wah-plugins
  (package
    (name "wah-plugins")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/WAH-plugins-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1wkbjarxdhjixkh7d5abralj11dj2xxg644fz3ycd7qyfgfvjfgd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA Autowah effect plugin")
    (description
     "This package provides a LADSPA plugin for a Wah effect with envelope
follower.")
    (license license:gpl2+)))

(define-public g2reverb
  (package
    (name "g2reverb")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/g2reverb-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "18wb8vj1kky5glr76s34awbi8qzplsmf3wjbd7a12hfv4j0bkwrj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure)
         (add-before 'install 'prepare-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib/ladspa"))
             #t))
         (add-after 'unpack 'override-target-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr") (assoc-ref outputs "out")))
             #t)))))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA stereo reverb plugin")
    (description
     "This package provides a LADSPA plugin for a stereo reverb effect.")
    (license license:gpl2+)))

(define-public fluidsynth
  (package
    (name "fluidsynth")
    (version "2.2.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FluidSynth/fluidsynth")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1061rdj69503spkd8vmfl3fqvyg4l41k5xcc4gw7niy31hnpnjmn"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-libdir
           (lambda _
             ;; Install libraries to /lib, not /lib64.
             (substitute* "CMakeLists.txt"
               (("LIB_SUFFIX \\$\\{_init_lib_suffix\\}")
                "LIB_SUFFIX \"\"")))))))
    (inputs
     (list ladspa))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; In Libs.private of fluidsynth.pc.
     (list alsa-lib
           glib
           jack-1
           lash
           libsndfile
           readline))
    (home-page "https://www.fluidsynth.org/")
    (synopsis "SoundFont synthesizer")
    (description
     "FluidSynth is a real-time software synthesizer based on the SoundFont 2
specifications.  FluidSynth reads and handles MIDI events from the MIDI input
device.  It is the software analogue of a MIDI synthesizer.  FluidSynth can
also play midifiles using a Soundfont.")
    (license license:lgpl2.1+)))

(define-public faad2
  (package
    (name "faad2")
    (version "2.8.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/faac/faad2-src/faad2-"
                       (version-major+minor version) ".0/"
                       "faad2-" version ".tar.gz"))
       (sha256
        (base32 "0va284hndhn0ynm4lyn219qw4y8wa4agfkqgwlaji7bqp6nkyp4q"))))
    (build-system gnu-build-system)
    (home-page "https://www.audiocoding.com/faad2.html")
    (synopsis "MPEG-4 and MPEG-2 AAC decoder")
    (description
     "FAAD2 is an MPEG-4 and MPEG-2 AAC decoder supporting LC, Main, LTP, SBR, -PS, and DAB+.")
    (license license:gpl2+)))

(define-public faust
  (package
    (name "faust")
    (version "0.9.90")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grame-cncm/faust")
                    (commit (string-append "v"
                                           (string-map (lambda (c)
                                                         (if (char=? c #\.) #\- c))
                                                       version)))))
              (file-name (string-append "faust-" version "-checkout"))
              (sha256
               (base32
                "0qc6iwjd3i80jdyjc186c6ywipmjzl8wlsp4050pbr56q4rlkd4z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; no "configure" script
         (delete 'configure)
         ;; Files appear under $out/share/faust that are read-only.  The
         ;; install phase tries to overwrite them and fails, so we change
         ;; the permissions first.
         (add-before 'install 'fix-permissions
           (lambda _
             (for-each (lambda (file)
                         (chmod file #o644))
                       (find-files "architecture/max-msp" ".*"))
             #t)))))
    (native-inputs
     (list unzip))
    (home-page "https://faust.grame.fr/")
    (synopsis "Signal processing language")
    (description
     "Faust is a programming language for realtime audio signal processing.")
    (license license:gpl2+)))

(define-public faust-2
  (package
    (inherit faust)
    (version "2.5.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/grame-cncm/faust/"
                                  "releases/download/" version
                                  "/faust-" version ".tar.gz"))
              (sha256
               (base32
                "1yz5jnr76hh7rmxkpdi7gyrw1wp4gyqfpq8zyl97qdi5ga5gjznq"))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments faust)
       ((#:make-flags flags)
        `(list (string-append "prefix=" (assoc-ref %outputs "out"))
               "world"))))
    (native-inputs
     `(("llvm" ,llvm-3.8)
       ("which" ,which)
       ("xxd" ,xxd)
       ("ctags" ,emacs-minimal)  ; for ctags
       ("pkg-config" ,pkg-config)))
    (inputs
     (list libsndfile libmicrohttpd ncurses openssl zlib))))

(define-public freepats
  (package
    (name "freepats")
    (version "20060219")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://freepats.zenvoid.org/freepats-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "12iw36rd94zirll96cd5k0va7p5hxmf2shvjlhzihcmjaw8flq82"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((out (string-append %output "/share/freepats")))
                     (setenv "PATH" (string-append
                                     (assoc-ref %build-inputs "bzip2") "/bin:"
                                     (assoc-ref %build-inputs "tar") "/bin"))
                     (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
                     (chdir "freepats")
                     ;; Use absolute pattern references
                     (substitute* "freepats.cfg"
                       (("Tone_000") (string-append out "/Tone_000"))
                       (("Drum_000") (string-append out "/Drum_000")))
                     (mkdir-p out)
                     (copy-recursively "." out)
                     #t))))
    (native-inputs
     (list tar bzip2))
    (home-page "http://freepats.zenvoid.org")
    (synopsis "GUS compatible patches for MIDI players")
    (description
     "FreePats is a project to create a free and open set of GUS compatible
patches that can be used with softsynths such as Timidity and WildMidi.")
    ;; GPLv2+ with exception for compositions using these patches.
    (license license:gpl2+)))

(define-public guitarix
  (package
    (name "guitarix")
    (version "0.43.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/guitarix/guitarix/guitarix2-"
                   version ".tar.xz"))
             (sha256
              (base32
               "1bsjlfd7x09p3iiljilyfbns6hpqn9cgp6psl4ccd6i1lwascfrm"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:configure-flags
       (list
        ;; Add the output lib directory to the RUNPATH.
        (string-append "--ldflags=-Wl,-rpath=" %output "/lib"))))
    (inputs
     (list libsndfile
           boost
           curl
           avahi
           eigen
           lv2
           lilv
           ladspa
           jack-1
           gtkmm-3
           gtk+
           fftwf
           lrdf
           zita-resampler
           zita-convolver))
    (native-inputs
     `(("gperf" ,gperf)
       ("faust" ,faust)
       ("intltool" ,intltool)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("sassc" ,sassc)))
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))
    (home-page "https://guitarix.org/")
    (synopsis "Virtual guitar amplifier")
    (description "Guitarix is a virtual guitar amplifier running JACK.
Guitarix takes the signal from your guitar as a mono-signal from your sound
card.  The input is processed by a main amp and a rack-section.  Both can be
routed separately and deliver a processed stereo-signal via JACK.  You may
fill the rack with effects from more than 25 built-in modules including stuff
from a simple noise gate to modulation effects like flanger, phaser or
auto-wah.")
    (license license:gpl2+)))

(define-public guitarix-lv2
  (package (inherit guitarix)
    (name "guitarix-lv2")
    (arguments
     (substitute-keyword-arguments (package-arguments guitarix)
       ((#:configure-flags flags)
        `(cons "--no-standalone" ,flags))))))

(define-public rakarrack
  (package
    (name "rakarrack")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/rakarrack/rakarrack/"
                                  "rakarrack-" version "/rakarrack-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1rpf63pdn54c4yg13k7cb1w1c7zsvl97c4qxcpz41c8l91xd55kn"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* '("src/process.C"
                                 "src/global.h")
                    (("#include <Fl/") "#include <FL/"))
                  #t))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-utils
           fltk
           libx11
           libxext
           libxfixes
           libxft
           libxrender
           libxpm
           fontconfig
           freetype
           jack-1
           alsa-lib
           libsndfile
           libsamplerate
           zlib))
    (home-page "http://rakarrack.sourceforge.net/")
    (synopsis "Audio effects processor")
    (description
     "Rakarrack is a richly featured multi-effects processor emulating a
guitar effects pedalboard.  Effects include compressor, expander, noise gate,
equalizers, exciter, flangers, chorus, various delay and reverb effects,
distortion modules and many more.  Most of the effects engine is built from
modules found in the excellent software synthesizer ZynAddSubFX.  Presets and
user interface are optimized for guitar, but Rakarrack processes signals in
stereo while it does not apply internal band-limiting filtering, and thus is
well suited to all musical instruments and vocals.")
    ;; The code is explicitly licensed under the GPL version 2 only.
    (license license:gpl2)))

(define-public ir
  (package
    (name "ir")
    (version "1.3.4")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/tomszilagyi/ir.lv2")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0svmjhg4r6wy5ci5rwz43ybll7yxjv7nnj7nyqscbzhr3gi5aib0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                              ; no tests
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "INSTDIR="
                            (assoc-ref %outputs "out") "/lib/lv2"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)        ; no configure script
         ;; See https://github.com/tomszilagyi/ir.lv2/pull/20
         (add-after 'unpack 'fix-type
           (lambda _
             (substitute* '("ir_gui.cc" "lv2_ui.h")
               (("_LV2UI_Descriptor") "LV2UI_Descriptor"))
             #t)))))
    (inputs
     (list libsndfile
           libsamplerate
           lv2
           glib
           gtk+-2
           zita-convolver))
    (native-inputs
     (list pkg-config))
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))
    (home-page "https://tomszilagyi.github.io/plugins/ir.lv2")
    (synopsis "LV2 convolution reverb")
    (description
     "IR is a low-latency, real-time, high performance signal convolver
especially for creating reverb effects.  It supports impulse responses with 1,
2 or 4 channels, in any soundfile format supported by libsndfile.")
    (license license:gpl2+)))

(define-public jack-1
  (package
    (name "jack")
    (version "0.125.0")
    (source
     (origin
       (method url-fetch)
       ;; jackaudio.org/downloads/jack-audio-connection-kit-0.125.0.tar.gz
       ;; no longer exists (404).  Use an unofficial mirror.
       (uri (string-append "https://crux.ster.zone/downloads/"
                           "jack-audio-connection-kit/"
                           "jack-audio-connection-kit-" version ".tar.gz"))
       (sha256
        (base32 "0i6l25dmfk2ji2lrakqq9icnwjxklgcjzzk65dmsff91z2zva5rm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-configure
                    (lambda _
                      (substitute* "configure"
                        ;; Install to <out/lib> regardless of platform.
                        (("libnn=lib64") "libnn=lib"))
                      #t)))))
    (inputs
     (list alsa-lib readline))
    ;; uuid.h is included in the JACK type headers
    ;; db.h is included in the libjack metadata headers
    (propagated-inputs
     `(("libuuid" ,util-linux "lib")
       ("bdb" ,bdb)))
    (native-inputs
     (list pkg-config))
    (home-page "https://jackaudio.org/")
    (synopsis "JACK audio connection kit")
    (description
     "JACK is a low-latency audio server.  It can connect a number of
different applications to an audio device, as well as allowing them to share
audio between themselves.  JACK is different from other audio server efforts
in that it has been designed from the ground up to be suitable for
professional audio work.  This means that it focuses on two key areas:
synchronous execution of all clients, and low latency operation.")
    ;; Most files are licensed under the GPL. However, the libjack/ tree is
    ;; licensed under the LGPL in order to allow for proprietary usage.
    (license (list license:gpl2+ license:lgpl2.1+))))

;; Packages depending on JACK should always prefer jack-1.  Both jack-1 and
;; jack-2 implement the same API.  JACK2 is provided primarily as a client
;; program for users who might benefit from the D-BUS features.
(define-public jack-2
  (package (inherit jack-1)
    (name "jack2")
    (version "1.9.14")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/jackaudio/jack2/releases/"
                                 "download/v" version "/jack2-"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "0z11hf55a6mi8h50hfz5wry9pshlwl4mzfwgslghdh40cwv342m2"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f  ; no check target
       #:configure-flags '("--dbus"
                           "--alsa")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-linkflags
           (lambda _
             ;; Ensure -lstdc++ is the tail of LDFLAGS or the simdtests.cpp
             ;; will not link with undefined reference to symbol
             ;; '__gxx_personality_v0@@CXXABI_1.3'
             (setenv "LDFLAGS" "-lstdc++")
             ;; Add $libdir to the RUNPATH of all the binaries.
             (substitute* "wscript"
               ((".*CFLAGS.*-Wall.*" m)
                (string-append m
                               "    conf.env.append_unique('LINKFLAGS',"
                               "'-Wl,-rpath=" %output "/lib')\n")))
             #t))
         (add-after 'install 'wrap-python-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'jack_control' runs with the correct PYTHONPATH.
             (let* ((out (assoc-ref outputs "out"))
                    (path (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/jack_control")
                 `("GUIX_PYTHONPATH" ":" prefix (,path))))
             #t)))))
    (inputs
     (list alsa-lib
           dbus
           expat
           libsamplerate
           opus
           python-dbus
           readline))
    (native-inputs
     (list pkg-config))
    ;; Most files are under GPLv2+, but some headers are under LGPLv2.1+
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public jalv
  (package
    (name "jalv")
    (version "1.6.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/jalv-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "05lycfq0f06zjp5xqvzjz9hx9kmqx72yng1lghh76hv63dw43lcj"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:python ,python-2))
    (inputs
     `(("lv2" ,lv2)
       ("lilv" ,lilv)
       ("suil" ,suil)
       ("gtk2" ,gtk+-2)
       ("gtk3" ,gtk+)
       ("gtkmm" ,gtkmm-2)
       ("qtbase" ,qtbase-5)
       ("jack" ,jack-1)))
    (native-inputs
     (list pkg-config))
    (home-page "https://drobilla.net/software/jalv/")
    (synopsis "Simple LV2 host for JACK")
    (description
     "Jalv is a simple but fully featured LV2 host for JACK.  It runs LV2
plugins and exposes their ports as JACK ports, essentially making any LV2
plugin function as a JACK application.")
    (license license:isc)))

(define-public ladspa
  (package
    (name "ladspa")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       ;; Since the official link is dead,
       ;; we download the tarball from Debian or Internet Archive.
       (uri (list (string-append "http://http.debian.net"
                                 "/debian/pool/main/l/ladspa-sdk/ladspa-sdk_"
                                 version ".orig.tar.gz")
                  (string-append "https://web.archive.org/web/20140717172251/"
                                 "http://www.ladspa.org/download/ladspa_sdk_"
                                 version ".tgz")))
       (sha256
        (base32
         "0srh5n2l63354bc0srcrv58rzjkn4gv8qjqzg8dnq3rs4m7kzvdm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; the "test" target is a listening test only
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (chdir "src")
             (let ((out (assoc-ref outputs "out")))
               (substitute* "makefile"
                 (("/usr/lib/ladspa/") (string-append out "/lib/ladspa/"))
                 (("/usr/include/")    (string-append out "/include/"))
                 (("/usr/bin/")        (string-append out "/bin/"))
                 (("-mkdirhier")       "mkdir -p")
                 (("^CC.*")            "CC = gcc\n")
                 (("^CPP.*")           "CPP = g++\n")))
             #t))
         (delete 'build))))
    ;; Since the home page is gone, we provide a link to the archived version.
    (home-page
     "https://web.archive.org/web/20140729190945/http://www.ladspa.org/")
    (synopsis "Linux Audio Developer's Simple Plugin API (LADSPA)")
    (description
     "LADSPA is a standard that allows software audio processors and effects
to be plugged into a wide range of audio synthesis and recording packages.")
    (license license:lgpl2.1+)))

(define-public lash
  (package
    (name "lash")
    (version "0.6.0-rc2")
    (source (origin
              (method url-fetch)
              ;; The tilde is not permitted in the builder name, but is used
              ;; in the tarball.
              (uri (string-append
                    "mirror://savannah/lash/lash-"
                    (string-join (string-split version #\-) "~")
                    ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "12z1vx3krrzsfccpah9xjs68900xvr7bw92wx8np5871i2yv47iw"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Glibc no longer includes Sun RPC support, so tell the build system
       ;; to use libtirpc instead.
       #:make-flags (list (string-append "CFLAGS=-I"
                                         (assoc-ref %build-inputs "libtirpc")
                                         "/include/tirpc -ltirpc"))
       #:phases
       (modify-phases %standard-phases
         ;; lashd embeds an ancient version of sigsegv so we just skip it
         (add-after 'unpack 'skip-lashd
           (lambda _
             (substitute* '("Makefile.am" "Makefile.in")
               (("lashd ") ""))
             #t)))
       #:configure-flags '("--disable-static")))
    (inputs
     `(("bdb" ,bdb)
       ("gtk" ,gtk+-2)
       ("jack" ,jack-1)
       ("libtirpc" ,libtirpc)
       ("readline" ,readline)
       ("python" ,python-2)))
    ;; According to pkg-config, packages depending on lash also need to have
    ;; at least the following packages declared as inputs.
    (propagated-inputs
     (list alsa-lib dbus libxml2))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.nongnu.org/lash/")
    (synopsis "Audio application session manager")
    (description
     "LASH is a session management system for audio applications.  It allows
you to save and restore audio sessions consisting of multiple interconneced
applications, restoring program state (i.e. loaded patches) and the
connections between them.")
    (license license:gpl2+)))

(define-public libbs2b
  (package
    (name "libbs2b")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bs2b/libbs2b/" version
                                  "/libbs2b-" version ".tar.lzma"))
              (sha256
               (base32
                "1mcc4gjkmphczjybnsrip3gq1f974knzys7x49bv197xk3fn8wdr"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libsndfile))
    (home-page "https://sourceforge.net/projects/bs2b/")
    (synopsis "Bauer stereophonic-to-binaural DSP")
    (description
     "The Bauer stereophonic-to-binaural DSP (bs2b) library and plugins is
designed to improve headphone listening of stereo audio records.  Recommended
for headphone prolonged listening to disable superstereo fatigue without
essential distortions.")
    (license license:expat)))

(define-public ladspa-bs2b
  (package
    (name "ladspa-bs2b")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/bs2b/plugins/LADSPA%20plugin/"
                    version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b4aipbj1ba5k99gbc7gmgy14sywyrjd8rpyqj5l905j0mjv8jg2"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list ladspa libbs2b))
    (home-page "https://sourceforge.net/projects/bs2b/")
    (synopsis "Bauer stereophonic-to-binaural DSP - LADSPA plugin")
    (description "The Bauer stereophonic-to-binaural DSP (bs2b) library and
plugins is designed to improve headphone listening of stereo audio records.
Recommended for headphone prolonged listening to disable superstereo fatigue
without essential distortions.  This package contains a LADSPA plugin for use
with applications that support them (e.g. PulseAudio).")
    (license license:gpl2+)))

(define-public liblo
  (package
    (name "liblo")
    (version "0.31")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/liblo/liblo/" version
                                 "/liblo-" version ".tar.gz"))
             (sha256
              (base32
               "0l67rkdhfa8cffa0nynql3lh2xlbn1454h6qxhjddp1029p48krb"))))
    (build-system gnu-build-system)
    (arguments
     `(;; liblo test FAILED
       ;; liblo server error 19 in setsockopt(IP_ADD_MEMBERSHIP): No such device
       #:tests? #f))
    (home-page "http://liblo.sourceforge.net")
    (synopsis "Implementation of the Open Sound Control protocol")
    (description
     "liblo is a lightweight library that provides an easy to use
implementation of the Open Sound Control (@dfn{OSC}) protocol.")
    (license license:lgpl2.1+)))

(define-public rtaudio
  (package
    (name "rtaudio")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/thestk/rtaudio")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "156c2dgh6jrsyfn1y89nslvaxm4yifmxridsb708yvkaym02w2l8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The header that pkg-config expects is include/rtaudio/RtAudio.h,
         ;; but this package installs it as include/RtAudio.h by default.
         (add-after 'install 'fix-inc-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (inc (string-append out "/include")))
               (mkdir-p (string-append inc "/rtaudio"))
               (rename-file (string-append inc "/RtAudio.h")
                            (string-append inc "/rtaudio/RtAudio.h"))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib jack-1 pulseaudio))
    (synopsis "Common API for real-time audio I/O")
    (description
     "RtAudio is a set of C++ classes that provides a common API for real-time
audio input/output.  It was designed with the following objectives:

@itemize
@item object-oriented C++ design
@item simple, common API across all supported platforms
@item only one source and one header file for easy inclusion in programming
projects
@item allow simultaneous multi-api support
@item support dynamic connection of devices
@item provide extensive audio device parameter control
@item allow audio device capability probing
@item automatic internal conversion for data format, channel number
compensation, (de)interleaving, and byte-swapping
@end itemize")
    (home-page "https://www.music.mcgill.ca/~gary/rtaudio/")
    ;; License is expat with a non-binding request to send modifications to
    ;; original developer.
    (license license:expat)))

(define-public python-pyaudio
  (package
    (name "python-pyaudio")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyAudio" version))
       (sha256
        (base32
         "0x7vdsigm7xgvyg3shd3lj113m8zqj2pxmrgdyj66kmnw0qdxgwk"))))
    (build-system python-build-system)
    (inputs
     (list portaudio))
    (home-page "https://people.csail.mit.edu/hubert/pyaudio/")
    (synopsis "Bindings for PortAudio v19")
    (description "This package provides bindings for PortAudio v19, the
cross-platform audio input/output stream library.")
    (license license:expat)))

(define-public python2-pyaudio
  (package-with-python2 python-pyaudio))

(define-public python-pyliblo
  (package
    (name "python-pyliblo")
    (version "0.10.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://das.nasophon.de/download/pyliblo-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "13vry6xhxm7adnbyj28w1kpwrh0kf7nw83cz1yq74wl21faz2rzw"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ;no tests
    (native-inputs
     (list python-cython))
    (inputs
     (list liblo))
    (home-page "http://das.nasophon.de/pyliblo/")
    (synopsis "Python bindings for liblo")
    (description
     "Pyliblo is a Python wrapper for the liblo Open Sound Control (OSC)
library.  It supports almost the complete functionality of liblo, allowing you
to send and receive OSC messages using a nice and simple Python API.  Also
included are the command line utilities @code{send_osc} and @code{dump_osc}.")
    (license license:lgpl2.1+)))

(define-public python2-pyliblo
  (package-with-python2 python-pyliblo))

(define-public python-soundfile
  (package
    (name "python-soundfile")
    (version "0.10.3.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "SoundFile" version))
       (sha256
        (base32
         "0yqhrfz7xkvqrwdxdx2ydy4h467sk7z3gf984y1x2cq7cm1gy329"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cffi python-numpy libsndfile))
    (native-inputs
     (list python-pytest))
    (arguments
     `(#:tests? #f ; missing OGG support
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "soundfile.py"
               (("_find_library\\('sndfile'\\)")
                (string-append "\"" (assoc-ref inputs "libsndfile")
                               "/lib/libsndfile.so\""))))))))
    (home-page "https://github.com/bastibe/SoundFile")
    (synopsis "Python bindings for libsndfile")
    (description "This package provides python bindings for libsndfile based on
CFFI and NumPy.")
    (license license:expat)))

(define-public python-python3-midi
  (package
    (name "python-python3-midi")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python3_midi" version))
       (sha256
        (base32
         "1z9j1w7mpn3xhkcpxmqm5rvmj6nffb5rf14bv7n3sdh07nf6n7sf"))))
    (build-system python-build-system)
    (home-page "https://github.com/NFJones/python3-midi")
    (synopsis "Python MIDI API")
    (description "This package provides a python API to read and write MIDI
files.")
    (license license:expat)))

(define-public audio-to-midi
  (package
    (name "audio-to-midi")
    (version "2020.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/NFJones/audio-to-midi")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "12wf17abn3psbsg2r2lk0xdnk8n5cd5rrvjlpxjnjfhd09n7qqgm"))))
    (build-system python-build-system)
    (propagated-inputs
      (list python-cffi
            python-cython
            python-numpy
            python-progressbar2
            python-pycparser
            python-python3-midi
            python-soundfile))
    (native-inputs
     (list libsndfile))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-versions
           (lambda _
             (substitute* "requirements.txt" (("==") ">=")))))))
    (home-page "https://github.com/NFJones/audio-to-midi")
    (synopsis "Convert audio to multichannel MIDI")
    (description "@command{audio-to-midi} converts audio files to multichannel
MIDI files.  It accomplishes this by performing FFTs on all channels of the
audio data at user-specified time steps.  It then separates the resulting
frequency analysis into equivalence classes which correspond to the twelve tone
scale; the volume of each class being the average volume of its constituent
frequencies.  This data is then formatted to MIDI and written to disk.")
    (license license:expat)))

(define-public lilv
  (package
    (name "lilv")
    (version "0.24.12")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.drobilla.net/lilv-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0qchfsyrsrp2pdpd59025kllycr04ddpzd03ha1iz70ci687g8r6"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-ldflags
          (lambda* (#:key outputs #:allow-other-keys)
            (setenv "LDFLAGS"
                    (string-append "-Wl,-rpath="
                                   (assoc-ref outputs "out") "/lib"))))
         (add-after 'unpack 'full-store-path-to-shared-library
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion "bindings/python"
               (substitute* "lilv.py"
                 (("liblilv-0.so") (string-append (assoc-ref outputs "out")
                                                  "/lib/liblilv-0.so")))))))))
    ;; Required by lilv-0.pc.
    (propagated-inputs
     (list lv2 serd sord sratom))
    (native-inputs
     (list python pkg-config))
    (home-page "https://drobilla.net/software/lilv")
    (synopsis "Library to simplify use of LV2 plugins in applications")
    (description
     "Lilv is a C library to make the use of LV2 plugins as simple as possible
for applications.  Lilv is the successor to SLV2, rewritten to be
significantly faster and have minimal dependencies.")
    (license license:isc)))

(define-public lv2
  (package
    (name "lv2")
    (version "1.18.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://lv2plug.in/spec/lv2-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0pp0n9x1rg8d4fw853z9cvfifjdi4bl85yjxxddqa1acfjy1z2af"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:configure-flags '("--no-plugins")))
    (inputs
     ;; Leaving off cairo and gtk+-2.0 which are needed for example plugins
     (list libsndfile))
    (native-inputs
     (list pkg-config))
    (home-page "https://lv2plug.in/")
    (synopsis "LV2 audio plugin specification")
    (description
     "LV2 is an open specification for audio plugins and host applications.
At its core, LV2 is a simple stable interface, accompanied by extensions which
add functionality to support the needs of increasingly powerful audio
software.")
    (license license:isc)))

(define-public lv2-mda-piano
  (package
    (name "lv2-mda-piano")
    (version "0.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "http://git.elephly.net/software/lv2-mdametapiano.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07lywf6lpfpndg3i9w752mmlg2hgn1bwp23h8b0mdj6awh67abqd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list
                     "TYPE=mdaPiano"
                     (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f ; no check target
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (inputs
     (list lv2 lvtk))
    (native-inputs
     (list pkg-config))
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))
    (home-page "https://elephly.net/lv2/mdapiano.html")
    (synopsis "LV2 port of the mda Piano plugin")
    (description "An LV2 port of the mda Piano VSTi.")
    (license license:gpl3+)))

(define-public lv2-mda-epiano
  (package (inherit lv2-mda-piano)
    (name "lv2-mda-epiano")
    (arguments
     `(#:make-flags (list
                     "TYPE=mdaEPiano"
                     (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f ; no check target
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://elephly.net/lv2/mdaepiano.html")
    (synopsis "LV2 port of the mda EPiano plugin")
    (description "An LV2 port of the mda EPiano VSTi.")))

(define-public lvtk
  (package
    (name "lvtk")
    (version "1.2.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/lvtk/lvtk")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1b01zvzl70ana6l1kn8fgyr7msnn3c7x61cgw7fdpp50322352p8"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f  ; no check target
       #:python ,python-2
       #:configure-flags
       (list (string-append "--boost-includes="
                            (assoc-ref %build-inputs "boost")
                            "/include"))))
    (inputs
     (list boost gtkmm-2 lv2))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/lvtk/lvtk")
    (synopsis "C++ libraries for LV2 plugins")
    (description
     "The LV2 Toolkit (LVTK) contains libraries that wrap the LV2 C API and
extensions into easy to use C++ classes.  It is the successor of
lv2-c++-tools.")
    (license license:gpl3+)))

(define-public openal
  (package
    (name "openal")
    (version "1.20.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://openal-soft.org/openal-releases/openal-soft-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0vax0b1lgd4212bpxa1rciz52d4mv3dkfvcbbhzw4cjp698v1kmn"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f  ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'use-full-library-paths
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "alc/backends/pulseaudio.cpp"
              (("#define PALIB \"libpulse\\.so\\.0\"")
               (string-append "#define PALIB \""
                              (assoc-ref inputs "pulseaudio")
                              "/lib/libpulse.so.0"
                              "\"")))
            (substitute* "alc/backends/alsa.cpp"
              (("LoadLib\\(\"libasound\\.so\\.2\"\\)")
               (string-append "LoadLib(\""
                              (assoc-ref inputs "alsa-lib")
                              "/lib/libasound.so.2"
                              "\")")))
            #t)))))
    (inputs
     (list alsa-lib pulseaudio))
    (synopsis "3D audio API")
    (description
     "OpenAL provides capabilities for playing audio in a virtual 3D
environment.  Distance attenuation, doppler shift, and directional sound
emitters are among the features handled by the API.  More advanced effects,
including air absorption, occlusion, and environmental reverb, are available
through the EFX extension.  It also facilitates streaming audio, multi-channel
buffers, and audio capture.")
    (home-page "https://openal-soft.org/")
    (license license:lgpl2.0+)))

(define-public freealut
  (package
    (name "freealut")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              ;; Upstream url is unclear, many systems use Fedora, there is also
              ;; https://github.com/vancegroup/freealut though the status of it
              ;; (official? unofficial?) is not clear.
              (uri (string-append
                    "https://pkgs.fedoraproject.org/repo/pkgs/" name "/" name "-"
                    version ".tar.gz" "/e089b28a0267faabdb6c079ee173664a/" name
                    "-" version ".tar.gz"))
              (sha256
               (base32
                "0kzlil6112x2429nw6mycmif8y6bxr2cwjcvp18vh6s7g63ymlb0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))  ; no check target
    (inputs
     (list openal))
    (synopsis "Free implementation of OpenAL's ALUT standard")
    (description "freealut is the OpenAL Utility Toolkit.")
    (home-page "https://kcat.strangesoft.net/openal.html")
    (license license:lgpl2.0)))

(define-public patchage
  (package
    (name "patchage")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/patchage-"
                                  version
                                  ".tar.bz2"))
              (sha256
               (base32
                "0gbakiw3mikgbvy3pssrmqmn7z5c7kp4vyaxj5rs4jnkscxgw9vw"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f))                    ; no check target
    (inputs
     (list alsa-lib
           boost
           jack-1
           ganv
           glibmm
           gtkmm-2
           dbus-glib))
    (native-inputs
     (list pkg-config))
    (home-page "https://drobilla.net/software/patchage.html")
    (synopsis "Modular patch bay for audio and MIDI systems")
    (description
     "Patchage is a modular patch bay for audio and MIDI systems based on JACK
and ALSA.")
    (license license:gpl3+)))

(define-public pcaudiolib
  (package
    (name "pcaudiolib")
    (version "1.1")
    (home-page "https://github.com/espeak-ng/pcaudiolib")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c55hlqqh0m7bcb3nlgv1s4a22s5bgczr1cakjh3767rjb10khi0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (native-inputs
     (list autoconf automake libtool pkg-config which))
    (inputs
     (list alsa-lib pulseaudio))
    (synopsis "Portable C audio library")
    (description
     "The Portable C Audio Library (pcaudiolib) provides a C@tie{}API to
different audio devices such as ALSA or PulseAudio.")
    (license (list license:gpl3+
                   ;; The bundled TPCircularBuffer uses a custom license.
                   (license:non-copyleft
                    "file://src/TPCircularBuffer/README.markdown")))))

(define-public qjackctl
  (package
    (name "qjackctl")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/qjackctl/qjackctl/"
                                  version "/qjackctl-" version ".tar.gz"))
              (sha256
               (base32
                "0sqni9ppwadc01fnyqj6lkwy30ql1vccqglv9imd3zdchffjpjir"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no check target
    (inputs
     (list jack-1 alsa-lib portaudio qtbase-5 qtx11extras))
    (native-inputs
     (list pkg-config qttools))
    (home-page "https://qjackctl.sourceforge.io/")
    (synopsis "Jack server control application")
    (description "Control a Jack server.  Allows you to plug various sources
into various outputs and to start, stop and configure jackd")
    (license license:gpl2+)))

(define-public qjackrcd
  (package
    (name "qjackrcd")
    (version "1.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/orouits/qjackrcd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l5iq2mkqd4gn9yr8xbh9qlpp1clcflazychl4vhdbz0bzq4c6al"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX="
                                    (assoc-ref outputs "out"))))))))
    (native-inputs
     (list qtbase-5)) ; for qmake
    (inputs
     (list jack-1 libsndfile qtbase-5))
    (home-page "https://sourceforge.net/projects/qjackrcd/")
    (synopsis "Stereo audio recorder for JACK")
    (description "QJackRcd is a simple graphical stereo recorder for JACK
supporting silence processing for automatic pause, file splitting, and
background file post-processing.")
    (license license:gpl2+)))

(define-public supercollider
  (package
    (name "supercollider")
    (version "3.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/supercollider/supercollider")
             (commit (string-append "Version-" version))
             ;; for nova-simd, nova-tt, hidapi, TLSF, oscpack
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0id522338a464j1slcspajwc7klypbc9qpigw5mqjhrw970wij5z"))
       (modules '((guix build utils)
                  (ice-9 ftw)))
       (snippet
        ;; The build system doesn't allow us to unbundle the following
        ;; libraries.  hidapi is also heavily patched and upstream not
        ;; actively maintained.
        '(let ((keep-dirs '("nova-simd" "nova-tt" "hidapi"
                            "TLSF-2.4.6" "oscpack_1_1_0" "." "..")))
           (with-directory-excursion "./external_libraries"
             (for-each
              delete-file-recursively
              (scandir "."
                       (lambda (x)
                         (and (eq? (stat:type (stat x)) 'directory)
                              (not (member (basename x) keep-dirs)))))))
           ;; To find the Guix provided ableton-link library.
           (substitute* "lang/CMakeLists.txt"
             (("include\\(\\.\\./external_libraries/link/\
AbletonLinkConfig\\.cmake\\)")
              "find_package(AbletonLink NAMES AbletonLink ableton-link \
link REQUIRED)"))))))
    (build-system cmake-build-system)
    (outputs
     '("out"                            ;core language
       "ide"))                          ;qt ide
    (arguments
     `(#:configure-flags '("-DSYSTEM_BOOST=ON"
                           "-DSYSTEM_YAMLCPP=ON"
                           "-DSC_QT=ON"
                           "-DCMAKE_BUILD_TYPE=Release"
                           "-DFORTIFY=ON"
                           ;; "-DLIBSCSYNTH=ON"   ; TODO: Re-enable?
                           "-DSC_EL=OFF") ;scel is packaged individually as emacs-scel
       #:phases
       (modify-phases %standard-phases
         ;; HOME must be defined otherwise supercollider throws a "ERROR:
         ;; Primitive '_FileMkDir' failed." error when generating the doc.
         ;; The graphical tests also hang without it.
         (add-after 'unpack 'set-home-directory
           (lambda _
             (setenv "HOME" (getcwd))))
         (add-after 'unpack 'patch-scclass-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (scclass-dir
                     (string-append out
                                    "/share/SuperCollider/SCClassLibrary")))
               (substitute* "lang/LangSource/SC_LanguageConfig.cpp"
                 (((string-append
                    "SC_Filesystem::instance\\(\\)\\.getDirectory"
                    "\\(DirName::Resource\\) / CLASS_LIB_DIR_NAME"))
                  (string-append "Path(\"" scclass-dir "\")"))))))
         (add-before 'build 'prepare-x
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0")))
         (add-before 'install 'install-ide
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (ide (assoc-ref outputs "ide"))
                    (scide "editors/sc-ide/scide"))
               (install-file scide
                             (string-append ide "/bin"))
               (delete-file scide)))))))
    (native-inputs
     (list ableton-link pkg-config qttools xorg-server-for-tests))
    (inputs (list jack-1
                  libsndfile
                  fftw
                  libxt
                  readline              ;readline support for sclang's CLI
                  alsa-lib              ;for sclang's MIDI interface
                  eudev                 ;for user interactions with devices
                  avahi                 ;zeroconf service discovery support
                  icu4c
                  boost
                  boost-sync
                  yaml-cpp
                  qtbase-5
                  qtdeclarative
                  qtsvg
                  qtwebchannel
                  qtwebsockets))
    (propagated-inputs                  ;to get native-search-path
     (list qtwebengine))
    (home-page "https://github.com/supercollider/supercollider")
    (synopsis "Synthesis engine and programming language")
    (description "SuperCollider is a synthesis engine (@code{scsynth} or
@code{supernova}) and programming language (@code{sclang}).  It can be used
for experimenting with sound synthesis and algorithmic composition.

SuperCollider requires jackd to be installed in your user profile and your
user must be allowed to access the realtime features of the kernel.  Search
for \"realtime\" in the index of the Guix manual to learn how to achieve this
using Guix System.")
    (license license:gpl2+)))

(define-public libshout-idjc
  (package
    (name "libshout-idjc")
    (version "2.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/libshoutidjc.idjc.p"
                           "/libshout-idjc-" version ".tar.gz"))
       (sha256
        (base32 "1r9z8ggxylr2ab0isaljbm574rplnlcb12758j994h54nh2vikwb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libogg libtheora libvorbis speex))
    (home-page "http://idjc.sourceforge.net/")
    (synopsis "Broadcast streaming library with IDJC extensions")
    (description "This package provides libshout plus IDJC extensions.")
    ;; GNU Library (not Lesser) General Public License.
    (license license:lgpl2.0+)))

(define-public raul
  (package
    (name "raul")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/raul-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "09ms40xc1x6qli6lxkwn5ibqh62nl9w7dq0b6jh1q2zvnrxwsd8b"))))
    (build-system waf-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f)) ; no check target
    (inputs
     (list glib boost))
    (native-inputs
     (list pkg-config))
    (home-page "https://drobilla.net/software/raul/")
    (synopsis "Real-time audio utility library")
    (description
     "Raul (Real-time Audio Utility Library) is a C++ utility library primarily
aimed at audio/musical applications.")
    (license license:gpl2+)))

(define-public raul-devel
  (let ((commit "4db870b2b20b0a608ec0283139056b836c5b1624")
        (revision "1"))
    (package (inherit raul)
      (name "raul")
      (version (string-append "0.8.9-" revision "."
                              (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.drobilla.net/raul.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "04fajrass3ymr72flx5js5vxc601ccrmx8ny8scp0rw7j0igyjdr")))))))

(define-public resample
  (package
    (name "resample")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ccrma.stanford.edu/~jos/gz/resample-"
                                  version
                                  ".tar.gz"))
              (sha256 (base32
                       "074zj8ydp05yy1hjcglfv3hkvj4cm50f9nralka1992pm6yf8yvy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config libtool))
    (synopsis "Sampling rate conversion and filter design utilities")
    (description "This package contains the @command{resample} and
@command{windowfilter} command line utilities.  The @command{resample} command
allows changing the sampling rate of a sound file, while the
@command{windowfilter} command allows designing Finite Impulse Response (FIR)
filters using the so-called @emph{window method}.")
    (home-page "https://ccrma.stanford.edu/~jos/resample/Free_Resampling_Software.html")
    (license license:lgpl2.1+)))

(define-public rubberband
  (package
    (name "rubberband")
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://bitbucket.org/breakfastquay/rubberband/get/v"
                              version
                              ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0462fmjnfqpv2qi0s6ny42drqarkr0xy9lw8frjmfgzyzl5n9294"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-jni-installation
           ;; ‘make install’ unconditionally installs librubberband-jni.so,
           ;; which is never built by ‘make all’.  Skip it.
           (lambda _
             (substitute* "Makefile.in"
               ((".*cp -f \\$\\(JNI_TARGET\\).*") ""))
             #t)))))
    (inputs
     (list ladspa libsamplerate vamp))
    (native-inputs
     (list pkg-config))
    (home-page "https://breakfastquay.com/rubberband/")
    (synopsis "Audio time-stretching and pitch-shifting library")
    (description
     "Rubber Band is a library and utility program that permits changing the
tempo and pitch of an audio recording independently of one another.")
    (license license:gpl2+)))

(define-public rtmidi
  (package
    (name "rtmidi")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.music.mcgill.ca/~gary/rtmidi"
                                  "/release/rtmidi-" version ".tar.gz"))
              (file-name (string-append "rtmidi-" version ".tar.gz"))
              (sha256
               (base32
                "1k962ljpnwyjw9jjiky2372khhri1wqvrj5qsalfpys31xqzw31p"))))
    (build-system gnu-build-system)
    (inputs
     (list jack-1 alsa-lib))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://www.music.mcgill.ca/~gary/rtmidi")
    (synopsis "Cross-platform MIDI library for C++")
    (description
     "RtMidi is a set of C++ classes (RtMidiIn, RtMidiOut, and API specific
classes) that provide a common cross-platform API for realtime MIDI
input/output.")
    (license license:expat)))

(define-public sratom
  (package
    (name "sratom")
    (version "0.6.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/sratom-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "178v90qvsp6lw4sqdmdz0bzyjkgwhv9m75ph1d1z8say5bv0p4gv"))))
    (build-system waf-build-system)
    (arguments `(#:tests? #f))          ;no check target
    (propagated-inputs
     ;; In Requires of sratom-0.pc.
     (list lv2 serd sord))
    (native-inputs
     (list pkg-config))
    (home-page "https://drobilla.net/software/sratom/")
    (synopsis "Library for serialising LV2 atoms to/from RDF")
    (description
     "Sratom is a library for serialising LV2 atoms to/from RDF, particularly
the Turtle syntax.")
    (license license:isc)))

(define-public suil
  (package
    (name "suil")
    (version "0.10.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/suil-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1ysbazqlbyxlzyr9zk7dj2mgb6pn0amllj2cd5g1m56wnzk0h3vm"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f))                    ;no check target
    (inputs
     (list lv2
           gtk+-2
           gtk+
           qtbase-5))
    (native-inputs
     (list pkg-config))
    (home-page "https://drobilla.net/software/suil/")
    (synopsis "Library for loading and wrapping LV2 plugin UIs")
    (description
     "Suil is a lightweight C library for loading and wrapping LV2 plugin UIs.

Suil makes it possible to load a UI of a toolkit in a host using another
toolkit.  The API is designed such that hosts do not need to explicitly
support specific toolkits – if Suil supports a particular toolkit, then UIs in
that toolkit will work in all hosts that use Suil automatically.

Suil currently supports every combination of Gtk, Qt, and X11.")
    (license license:isc)))

(define-public libebur128
  (package
    (name "libebur128")
    (version "1.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jiixyj/libebur128")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xkpz5rzz1j1srhprbh89669gw8z5f1njpvcnxqgf7qax69vd8sh"))))
    (build-system cmake-build-system)
    (arguments
     `(;; Tests require proprietary .wav files. See
       ;; https://github.com/jiixyj/libebur128/issues/82.
       #:tests? #f
       #:configure-flags '("-DBUILD_STATIC_LIBS=OFF")))
    (home-page "https://github.com/jiixyj/libebur128")
    (synopsis "Library implementing the EBU R 128 loudness standard")
    (description
     "@code{libebur128} is a C library that implements the EBU R 128 standard
for loudness normalisation.")
    (license license:expat)))

(define-public timidity++
  (package
    (name "timidity++")
    (version "2.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/timidity/TiMidity++"
                                  "/TiMidity++-" version
                                  "/TiMidity++-" version ".tar.bz2"))
              (sha256
               (base32
                "1xf8n6dqzvi6nr2asags12ijbj1lwk1hgl3s27vm2szib8ww07qn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--enable-audio=alsa,flac,jack,ao,vorbis,speex"
             "--enable-ncurses"
             "--enable-server"
             "--enable-alsaseq"
             (string-append "--with-default-path="
                            (assoc-ref %outputs "out") "/etc/timidity"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-config
           (lambda _
             (let ((out (string-append (assoc-ref %outputs "out")
                                       "/etc/timidity")))
               (mkdir-p out)
               (call-with-output-file
                   (string-append out "/timidity.cfg")
                 (lambda (port)
                   (format port (string-append "source "
                                               (assoc-ref %build-inputs "freepats")
                                               "/share/freepats/freepats.cfg")))))
             #t)))))
    (inputs
     (list alsa-lib
           ao
           flac
           jack-1
           libogg
           libvorbis
           speex
           ncurses
           freepats))
    (native-inputs
     (list pkg-config))
    (home-page "http://timidity.sourceforge.net/")
    (synopsis "Software synthesizer for playing MIDI files")
    (description
     "TiMidity++ is a software synthesizer.  It can play MIDI files by
converting them into PCM waveform data; give it a MIDI data along with digital
instrument data files, then it synthesizes them in real-time, and plays.  It
can not only play sounds, but also can save the generated waveforms into hard
disks as various audio file formats.")
    (license license:gpl2+)))

(define-public vamp
  (package
    (name "vamp")
    (version "2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://code.soundsoftware.ac.uk"
                    "/attachments/download/1520/vamp-plugin-sdk-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0pzq0yy2kdl3jgfc2aqmh5m55nk1ild0hq8mydiiw35c6y0mglyh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-libvamp-hostsdk.la
           (lambda* (#:key outputs #:allow-other-keys)
             ;; https://bugs.launchpad.net/ubuntu/+source/vamp-plugin-sdk/+bug/1253656
             (for-each delete-file
                       (let ((out (assoc-ref outputs "out")))
                         (list (string-append out "/lib/libvamp-sdk.la")
                               (string-append out "/lib/libvamp-hostsdk.la"))))
             #t)))))
    (inputs
     (list libsndfile))
    (native-inputs
     (list pkg-config))
    (home-page "https://vamp-plugins.org")
    (synopsis "Modular and extensible audio processing system")
    (description
     "Vamp is an audio processing plugin system for plugins that extract
descriptive information from audio data — typically referred to as audio
analysis plugins or audio feature extraction plugins.")
    (license
     (license:x11-style
      "https://code.soundsoftware.ac.uk/projects/vamp-plugin-sdk/repository/entry/COPYING"))))

(define-public libsbsms
  (package
    (name "libsbsms")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sbsms/sbsms/" version
                           "/libsbsms-" version ".tar.gz"))
       (sha256
        (base32 "1vmf84iy4dkwxv887grnlsfk43fmhd9gbg26gc2kgcv40sbkvayf"))))
    (build-system gnu-build-system)
    (native-inputs (list automake))
    (arguments
     `(#:configure-flags
       ;; Disable the use of SSE unless on x86_64.
       ,(if (not (string-prefix? "x86_64" (or (%current-target-system)
                                              (%current-system))))
            ''("--disable-sse")
            ''())
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'fix-ar-lib-path
          (lambda* (#:key inputs #:allow-other-keys)
            ;; Originally a symlink to '/usr/local/share/automake-1.12/ar-lib'.
            (delete-file "ar-lib")
            (symlink
             (search-input-file inputs
                                (string-append "/share/automake-"
                                               ,(package-version automake)
                                               "/ar-lib"))
             "ar-lib")
            #t)))))
    (home-page "http://sbsms.sourceforge.net/")
    (synopsis "Library for time stretching and pitch scaling of audio")
    (description
     "SBSMS (Subband Sinusoidal Modeling Synthesis) is software for time
stretching and pitch scaling of audio.  This package contains the library.")
    ;; There is no explicit declaration of a license, but a COPYING file
    ;; containing gpl2.
    (license license:gpl2)))

(define-public libkeyfinder
  (package
    (name "libkeyfinder")
    (version "2.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mixxxdj/libkeyfinder")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s7nqjmv44q5qjynfcs0j6h4a6qcz4mxzandkkdjjbnwv5rxc3zg"))))
    (build-system cmake-build-system)
    (native-inputs
     (list catch-framework2))
    (inputs
     (list fftw))
    (home-page "https://mixxxdj.github.io/libkeyfinder/")
    (synopsis "Musical key detection for digital audio")
    (description
     "@code{libkeyfinder} is a small C++11 library for estimating the musical
key of digital audio.")
    (license license:gpl3+)))

(define-public wavpack
  (package
    (name "wavpack")
    (version "5.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dbry/WavPack/releases/download/"
                           version "/wavpack-" version ".tar.xz"))
       (sha256
        (base32 "0ycbqarw25x7208jilh86vwwiqklr7f617jps9mllqc659mnmpjb"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--disable-static"
             "--enable-tests")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./cli/wvtest" "--default" "--short"))
             #t)))))
    (home-page "https://www.wavpack.com/")
    (synopsis "Hybrid lossless audio codec")
    (description
     "WavPack is an audio compression format with lossless, lossy and hybrid
compression modes.  This package contains command-line programs and library to
encode and decode wavpack files.")
    (license license:bsd-3)))

(define-public libmodplug
  (package
    (name "libmodplug")
    (version "0.8.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/modplug-xmms/"
                    name "/" version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pnri98a603xk47smnxr551svbmgbzcw018mq1k6srbrq6kaaz25"))))
    (build-system gnu-build-system)
    (home-page "http://modplug-xmms.sourceforge.net/")
    (synopsis "Mod file playing library")
    (description
     "Libmodplug renders mod music files as raw audio data, for playing or
conversion.  mod, .s3m, .it, .xm, and a number of lesser-known formats are
supported.  Optional features include high-quality resampling, bass expansion,
surround and reverb.")
    (license license:public-domain)))

(define-public libxmp
  (package
    (name "libxmp")
    (version "4.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xmp/libxmp/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kycz4jsyvmf7ny9227b497wc7y5ligydi6fvvldmkf8hk63ad9m"))))
    (build-system gnu-build-system)
    (home-page "http://xmp.sourceforge.net/")
    (synopsis "Module player library")
    (description
     "Libxmp is a library that renders module files to PCM data.  It supports
over 90 mainstream and obscure module formats including Protracker (MOD),
Scream Tracker 3 (S3M), Fast Tracker II (XM), and Impulse Tracker (IT).")
    (license license:lgpl2.1+)))

(define-public xmp
  (package
    (name "xmp")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xmp/xmp/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "17i8fc7x7yn3z1x963xp9iv108gxfakxmdgmpv3mlm438w3n3g8x"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libxmp pulseaudio))
    (home-page "http://xmp.sourceforge.net/")
    (synopsis "Extended module player")
    (description
     "Xmp is a portable module player that plays over 90 mainstream and
obscure module formats, including Protracker MOD, Fasttracker II XM, Scream
Tracker 3 S3M and Impulse Tracker IT files.")
    (license license:gpl2+)))

(define-public soundtouch
  (package
    (name "soundtouch")
    (version "2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/soundtouch/soundtouch.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12i6yg8vvqwyk412lxl2krbfby6hnxld8qxy0k4m5xp4g94jiq4p"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool file))
    (home-page "https://www.surina.net/soundtouch/")
    (synopsis
     "Audio processing library for changing tempo, pitch and playback rate")
    (description
     "SoundTouch is an audio processing library for changing the tempo, pitch
and playback rates of audio streams or audio files.  It is intended for
application developers writing sound processing tools that require tempo/pitch
control functionality, or just for playing around with the sound effects.")
    (license license:lgpl2.1+)))

(define-public sox
  (package
    (name "sox")
    (version "14.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/sox/sox/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "170lx90r1nlnb2j6lg00524iwvqy72p48vii4xc5prrh8dnrb9l1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; The upstream asks to identify the distribution to diagnose SoX
       ;; bug reports.
       '("--with-distro=Guix System Distribution")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           ao
           flac
           lame
           libid3tag
           libltdl
           libmad
           libpng
           libvorbis
           pulseaudio))
    (home-page "http://sox.sourceforge.net")
    (synopsis "Sound processing utility")
    (description
     "SoX (Sound eXchange) is a command line utility that can convert
various formats of computer audio files to other formats.  It can also
apply various effects to these sound files, and, as an added bonus, SoX
can play and record audio files.")
    ;; sox.c is distributed under GPL, while the files that make up
    ;; libsox are licensed under LGPL.
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public soxr
  (package
    (name "soxr")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/soxr/soxr-" version
                       "-Source.tar.xz"))
       (sha256
        (base32 "12aql6svkplxq5fjycar18863hcq84c5kx8g6f4rj0lcvigw24di"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ;no 'check' target
    (home-page "https://sourceforge.net/p/soxr/wiki/Home/")
    (synopsis "One-dimensional sample-rate conversion library")
    (description
     "The SoX Resampler library (libsoxr) performs one-dimensional sample-rate
conversion.  It may be used, for example, to resample PCM-encoded audio.")
    (license license:lgpl2.1+)))

(define-public twolame
  (package
    (name "twolame")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/twolame/twolame/" version
                           "/twolame-" version ".tar.gz"))
       (sha256
        (base32 "0zf8sxqb02w07ah55b1y7ab643zmpbhn62spa9pqra0rc17l4dfc"))))
    (build-system gnu-build-system)
    (inputs
     (list libsndfile))
    (native-inputs
     (list perl which))               ;used in tests/test.pl
    (home-page "https://www.twolame.org/")
    (synopsis "MPEG Audio Layer 2 (MP2) encoder")
    (description
     "TwoLAME is an optimised MPEG Audio Layer 2 (MP2) encoder based on
tooLAME by Mike Cheng, which in turn is based upon the ISO dist10 code and
portions of LAME.")
    (license license:lgpl2.1+)))

(define-public portaudio
  (package
    (name "portaudio")
    (version "190600.20161030")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.portaudio.com/archives/pa_stable_v"
             (string-map (lambda (c) (if (char=? c #\.) #\_ c)) version)
             ".tgz"))
       (sha256
        (base32 "04qmin6nj144b8qb9kkd9a52xfvm0qdgm8bg8jbl7s3frmyiv8pm"))
       (patches (search-patches "portaudio-audacity-compat.patch"))))
    (build-system gnu-build-system)
    (inputs
     ;; TODO: Add ASIHPI.
     (list alsa-lib jack-1))
    ;; Autoreconf is necessary because the audacity-compat patch modifies .in
    ;; files.
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (arguments
     '(#:tests? #f                      ;no 'check' target
       #:parallel-build? #f             ;fails on some systems
       #:configure-flags '("--with-pic"
                           "--enable-cxx"
                           ;; XXX: The following prevents a build error
                           ;; because of missing depcomp when C++ bindings are
                           ;; requested.
                           "--disable-dependency-tracking")
       #:phases
       (modify-phases %standard-phases
         ;; This is needed for linking the static libraries
         (add-after 'unpack 'build-only-position-independent-code
           (lambda _
             (substitute* "configure.in"
               (("AC_PROG_LIBTOOL" m)
                (string-append m "\nAM_PROG_AR\nLT_INIT([pic-only])")))
             (delete-file "configure")
             #t))
         ;; Some headers are not installed by default, but are needed by
         ;; packages like Kaldi.
         (add-after 'install 'install-missing-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "src/common/pa_ringbuffer.h"
                           (string-append (assoc-ref outputs "out") "/include"))
             #t)))))
    (home-page "http://www.portaudio.com/")
    (synopsis "Audio I/O library")
    (description
     "PortAudio is a portable C/C++ audio I/O library providing a simple API
to record and/or play sound using a callback function or a blocking read/write
interface.")
    (license license:expat)))

(define-public qsynth
  (package
    (name "qsynth")
    (version "0.5.7")
    (source
     (origin
       (method url-fetch)
       (uri (list
              (string-append "mirror://sourceforge/qsynth/qsynth/" version
                             "/qsynth-" version ".tar.gz")
              (string-append "mirror://sourceforge/qsynth/qsynth (attic)"
                             "/qsynth-" version ".tar.gz")))
       (sha256
        (base32 "18im4w8agj60nkppwbkxqnhpp13z5li3w30kklv4lgs20rvgbvl6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no "check" phase
    (native-inputs
     (list qttools pkg-config))
    (inputs
     (list fluidsynth qtbase-5 qtx11extras))
    (home-page "https://qsynth.sourceforge.io")
    (synopsis "Graphical user interface for FluidSynth")
    (description
     "Qsynth is a GUI front-end application for the FluidSynth SoundFont
synthesizer written in C++.")
    (license license:gpl2+)))

(define-public rsound
  (package
    (name "rsound")
    (version "1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Themaister/RSound")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gspmr3klwnq98h17p5hc6ifygya4p80g4g8r7a1qavm3mv19waf"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib
           jack-1
           ao
           libsamplerate
           openal
           portaudio
           pulseaudio))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (invoke "./configure"
                     (string-append "--prefix=" (assoc-ref outputs "out"))))))
       ;; No 'check' target.
       #:tests? #f))
    (home-page "https://themaister.net/rsound.html")
    (synopsis "Networked audio system")
    (description
     "RSound allows you to send audio from an application and transfer it
directly to a different computer on your LAN network.  It is an audio daemon
with a much different focus than most other audio daemons.")
    (license license:gpl3+)))

(define-public xjackfreak
  (package
    (name "xjackfreak")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/johnhldavis/xjackfreak")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18c546qidbrj0f5wfiq5llii2192xpln0ab3r4vpr7i3wybxqjfz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "docdir=" (assoc-ref %outputs "out")
                            "/share/doc/xjackfreak"))))
    (inputs
     (list jack-1 libx11 libxt libxext))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/johnhldavis/xjackfreak")
    (synopsis "JACK audio frequency analyzer and display")
    (description
     "XJackFreak is an audio analysis and equalizing tool for the Jack Audio
Connection Kit.  It can display the FFT of any input, modify it and output the
result.")
    (license license:gpl3+)))

(define-public zita-convolver
  (package
    (name "zita-convolver")
    (version "4.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/zita-convolver-"
                    version ".tar.bz2"))
              (snippet
               ;; Don't optimize for a specific processor architecture.
               '(begin
                  (substitute* "source/Makefile"
                    (("^CXXFLAGS \\+= -march=native") ""))
                  #t))
              (modules '((guix build utils)))
              (sha256
               (base32
                "0prji66p86z2bzminywkwchr5bfgxcg2i8y803pydd1hzf2198cs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "SUFFIX="))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile-and-enter-directory
           (lambda _
             (substitute* "source/Makefile"
               (("ldconfig") "true")
               (("^LIBDIR =.*") "LIBDIR = lib\n"))
             (chdir "source")
             #t))
         (add-after 'install 'install-symlink
           (lambda _
             (symlink "libzita-convolver.so"
                      (string-append (assoc-ref %outputs "out")
                                     "/lib/libzita-convolver.so.4"))
             #t))
         ;; no configure script
         (delete 'configure))))
    (inputs (list fftwf))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "Fast, partitioned convolution engine library")
    (description
     "Zita convolver is a C++ library providing a real-time convolution
engine.")
    (license license:gpl3+)))

(define-public zita-resampler
  (package
    (name "zita-resampler")
    (version "1.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/zita-resampler-"
                    version ".tar.bz2"))
              (snippet
               ;; Don't optimize for a specific processor architecture.
               '(begin
                  (substitute* '("apps/Makefile" "source/Makefile")
                    (("^CXXFLAGS \\+= -march=native") ""))
                  #t))
              (modules '((guix build utils)))
              (sha256
               (base32
                "1my5k2dh2dkvjp6xjnf9qy6i7s28z13kw1n9pwa4a2cpwbzawfr3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "SUFFIX="))
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-makefile-and-enter-directory
          (lambda _
            (substitute* "source/Makefile"
              (("ldconfig") "true")
              (("^LIBDIR =.*") "LIBDIR = lib\n"))
            (chdir "source")
            #t))
         (add-after
          'install 'install-symlink
          (lambda _
            (symlink "libzita-resampler.so"
                     (string-append (assoc-ref %outputs "out")
                                    "/lib/libzita-resampler.so.1"))
            #t))
         ;; no configure script
         (delete 'configure))))
    (home-page "https://kokkinizita.linuxaudio.org/linuxaudio/zita-resampler/resampler.html")
    (synopsis "C++ library for resampling audio signals")
    (description
     "Libzita-resampler is a C++ library for resampling audio signals.  It is
designed to be used within a real-time processing context, to be fast, and to
provide high-quality sample rate conversion.")
    (license license:gpl3+)))

(define-public zita-alsa-pcmi
  (package
    (name "zita-alsa-pcmi")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/zita-alsa-pcmi-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "12d7vdg74yh21w69qi0wg57iz4876j94qbiq09bvscih6xz9y78s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "SUFFIX="))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile-and-enter-directory
           (lambda _
             (substitute* "source/Makefile"
               (("ldconfig") "true")
               (("^LIBDIR =.*") "LIBDIR = lib\n"))
             (chdir "source")
             #t))
         (add-after 'install 'install-symlink
           (lambda _
             (symlink "libzita-alsa-pcmi.so"
                      (string-append (assoc-ref %outputs "out")
                                     "/lib/libzita-alsa-pcmi.so.0"))
             #t))
          ;; no configure script
          (delete 'configure))))
    (inputs
     (list alsa-lib fftw))
    (home-page "https://kokkinizita.linuxaudio.org")
    (synopsis "C++ wrapper around the ALSA API")
    (description
     "Zita-alsa-pcmi is a C++ wrapper around the ALSA API.  It provides easy
access to ALSA PCM devices, taking care of the many functions required to
open, initialise and use a hw: device in mmap mode, and providing floating
point audio data.")
    (license license:gpl3+)))

(define-public cuetools
  (package
    (name "cuetools")
    (version "1.4.1")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/svend/cuetools")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "02ksv1ahf1v4cr2xbclsfv5x17m9ivzbssb5r8xjm97yh8a7spa3"))))
    (build-system gnu-build-system)
    ;; The source checkout is not bootstrapped.
    (native-inputs
     (list autoconf automake flex bison))
    (synopsis "Cue and toc file parsers and utilities")
    (description "Cuetools is a set of programs that are useful for manipulating
and using CUE sheet (cue) files and Table of Contents (toc) files.  CUE and TOC
files are a way to represent the layout of a data or audio CD in a
machine-readable ASCII format.")
    (home-page "https://github.com/svend/cuetools")
    (license license:gpl2+)))

(define-public mp3guessenc
  (package
    (name "mp3guessenc")
    (version "0.27.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/mp3guessenc/mp3guessenc-"
                           (version-major+minor version) "/mp3guessenc-"
                           version ".tar.gz"))
       (sha256
        (base32 "1fa3sbwwn4p2v1749lzy040bfy1xfd574mf2frwgg9ikgk3vlb3c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure phase
    (home-page "https://mp3guessenc.sourceforge.io")
    (synopsis "Analyze MPEG layer I/II/III files")
    (description "mp3guessenc is a command line utility that tries to detect the
encoder used for an MPEG Layer III (MP3) file, as well as scan any MPEG audio
file (any layer) and print a lot of useful information.")
    (license license:lgpl2.1+)))

(define-public shntool
  (package
    (name "shntool")
    (version "3.0.10")
    (source (origin
             (method url-fetch)
             (uri (list
                    (string-append "http://etree.org/shnutils/shntool/dist/src/"
                                   "shntool-" version ".tar.gz")
                    (string-append "mirror://debian/pool/main/s/shntool/shntool_"
                                   version ".orig.tar.gz")))
             (sha256
              (base32
               "00i1rbjaaws3drkhiczaign3lnbhr161b7rbnjr8z83w8yn2wc3l"))))
    (build-system gnu-build-system)
    (synopsis "WAVE audio data processing tool")
    (description "shntool is a multi-purpose WAVE data processing and reporting
utility.  File formats are abstracted from its core, so it can process any file
that contains WAVE data, compressed or not---provided there exists a format
module to handle that particular file type.  It can also generate CUE files, and
use them split WAVE data into multiple files.")
    (home-page "http://etree.org/shnutils/shntool/")
    ;; 'install-sh' bears the x11 license
    (license (list license:gpl2+ license:x11))))

(define-public dcadec
  (package
    (name "dcadec")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/foo86/dcadec")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07nd0ajizrp1w02bsyfcv18431r8m8rq8gjfmz9wmckpg7cxj2hs"))))
    (build-system gnu-build-system)
    (arguments
     ;; Test files are missing: https://github.com/foo86/dcadec/issues/53
     `(#:tests? #f
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             ;; Build shared library.
             "CONFIG_SHARED=1"
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             ;; Set proper runpath.
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         ;; No configure script, just a hand-written Makefile.
         (delete 'configure))))
    (synopsis "DTS Coherent Acoustics decoder")
    (description "Dcadec is a DTS Coherent Acoustics surround sound decoder
with support for HD extensions.")
    (home-page "https://github.com/foo86/dcadec")
    (license license:lgpl2.1+)))

(define-public drc
  (package
    (name "drc")
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/drc-fir/drc-fir/"
                           version "/drc-" version ".tar.gz"))
       (sha256
        (base32
         "08ljj4776pjx119zjmfqa8w56bf7x0m7spmi27yk1m455bmiglrj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false ;there are none
      #:make-flags
      #~(list (string-append "INSTALL_PREFIX=" #$output)
              "-C" "source")
      #:phases
      '(modify-phases %standard-phases
         (delete 'configure))))
    (inputs (list fftw))
    (home-page "http://drc-fir.sourceforge.net/")
    (synopsis "Digital room correction")
    (description
     "DRC is a program used to generate correction filters for acoustic
compensation of HiFi and audio systems in general, including listening room
compensation.  DRC generates just the FIR correction filters, which can be
used with a real time or offline convolver to provide real time or offline
correction.  DRC doesn't provide convolution features, and provides only some
simplified, although really accurate, measuring tools.")
    (license license:gpl2+)))

(define-public bs1770gain
  (package
    (name "bs1770gain")
    (version "0.7.0")
    (home-page "https://manpages.debian.org/sid/bs1770gain/bs1770gain.1.en.html")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/bs1770gain/bs1770gain/"
                           version "/bs1770gain-" version ".tar.gz"))
       (sha256
        (base32 "0a2dcaxvxy5m3a5sb1lhplsymvz3ypaiidc5xn9qy01h53zvyvkp"))
       (modules '((guix build utils)))
       (snippet
        `(begin
           ;; XXX
           (substitute* (find-files "." "\\.[ch]$")
             (("^ \\* N..o.*") ""))
           (substitute* "libbg/bgx.c"
             (("#define BG.* ") "#define BS ")
             (("BG.*NO?.*N.*S.*E.*N.*SE?") "NO")
             (("\"( #|N).*\"") "\"\""))
           (substitute* (list "config.h"
                              "configure.ac"
                              "configure")
             (("https?://bs1770gain[^/]*/")
              ,home-page))
           #t))))
    (build-system gnu-build-system)
    (inputs (list ffmpeg sox))
    (synopsis "Tool to adjust loudness of media files")
    (description
     "BS1770GAIN is a loudness scanner compliant with ITU-R BS.1770 and its
flavors EBU R128, ATSC A/85, and ReplayGain 2.0.  It helps normalizing the
loudness of audio and video files to the same level.")
    (license license:gpl2+)))

(define-public filteraudio
  (let ((revision "1")
        (commit "2fc669581e2a0ff87fba8de85861b49133306094"))
    (package
      (name "filteraudio")
      (version (string-append "0.0.0-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/irungentoo/filter_audio")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0hbb290n3wb23f2k692a6bhc23nnqmxqi9sc9j15pnya8wifw64g"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" %output)
                            (string-append "CC=" ,(cc-for-target)))
         #:tests? #f ; No tests
         #:phases
         (modify-phases %standard-phases
           ;; No configure script
           (delete 'configure))))
      (synopsis "Lightweight audio filtering library")
      (description "An easy to use audio filtering library made from webrtc
code, used in @code{libtoxcore}.")
      (home-page "https://github.com/irungentoo/filter_audio")
      (license license:bsd-3))))

(define-public gsm
  (package
    (name "gsm")
    (version "1.0.19")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "http://www.quut.com/" name "/" name
                       "-" version ".tar.gz"))
       (sha256
        (base32
         "1xkha9ss5g5qnfaybi8il0mcvp8knwg9plgh8404vh58d0pna0s9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "tst"
       #:make-flags (list (string-append "INSTALL_ROOT=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-fpic-ccflag
           (lambda _
             ;; The -fPIC compiler option is needed when building
             ;; mediastreamer.
             (substitute* "Makefile"
               (("^CCFLAGS.*" all)
                (string-append all "CCFLAGS += -fPIC\n")))
             #t))
         (add-before 'install 'pre-install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (mkdir-p (string-append out "/inc"))
               (mkdir-p (string-append out "/man"))
               (mkdir-p (string-append out "/man/man1"))
               (mkdir-p (string-append out "/man/man3"))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/lib")))
             #t))
         (add-after 'install 'post-install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (rename-file (string-append out "/inc")
                            (string-append out "/include"))
               (mkdir-p (string-append out "/include/gsm"))
               (copy-recursively "inc"
                                 (string-append out "/include/gsm")))
             #t))
         (delete 'configure))))         ; no configure script
    (synopsis "GSM 06.10 lossy speech compression library")
    (description "This C library provides an encoder and a decoder for the GSM
06.10 RPE-LTP lossy speech compression algorithm.")
    (home-page "http://quut.com/gsm/")
    (license (license:non-copyleft "file://COPYRIGHT"))))

(define-public python-pyalsaaudio
  (package
    (name "python-pyalsaaudio")
    (version "0.8.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyalsaaudio" version))
              (sha256
               (base32
                "1180ypn9596rq4b7y7dyv627j1q0fqilmkkrckclnzsdakdgis44"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                   ; tests require access to ALSA devices.
    (inputs
     (list alsa-lib))
    (home-page "https://larsimmisch.github.io/pyalsaaudio/")
    (synopsis "ALSA wrappers for Python")
    (description
     "This package contains wrappers for accessing the ALSA API from Python.
It is currently fairly complete for PCM devices, and has some support for
mixers.")
    (license license:psfl)))

(define-public python2-pyalsaaudio
  (package-with-python2 python-pyalsaaudio))

(define-public ldacbt
  (package
    (name "ldacbt")
    (version "2.0.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/EHfive/ldacBT"
                                  "/releases/download/v" version
                                  "/ldacBT-" version ".tar.gz"))
              (sha256
               (base32
                "1d65dms4klzql29abi15i90f41h523kl6mxrz9hi6p5vg37fxn2b"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (home-page "https://github.com/EHfive/ldacBT/")
    (synopsis "LDAC Bluetooth encoder and ABR library")
    (description "This package provides an encoder for the LDAC
high-resolution Bluetooth audio streaming codec for streaming at up to 990
kbps at 24 bit/96 kHz.")
    (license license:asl2.0)))

(define-public bluez-alsa
  (package
    (name "bluez-alsa")
    (version "3.0.0")
    (source (origin
              ;; The tarballs are mere snapshots and don't contain a
              ;; bootstrapped build system.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Arkq/bluez-alsa")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jlsgxyqfhncfhx1sy3ry0dp6p95kd4agh7g2b7g51h0c4cv74h8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-ldac"
             (string-append "--with-alsaplugindir="
                            (assoc-ref %outputs "out")
                            "/lib/alsa-lib")
             (string-append "--with-dbusconfdir="
                            (assoc-ref %outputs "out")
                            "/etc/dbus-1/system.d"))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list alsa-lib
           bluez
           dbus
           glib
           ldacbt
           libbsd
           ncurses
           ortp
           sbc))
    (home-page "https://github.com/Arkq/bluez-alsa")
    (synopsis "Bluetooth ALSA backend")
    (description "This project is a rebirth of a direct integration between
Bluez and ALSA.  Since Bluez >= 5, the built-in integration has been removed
in favor of 3rd party audio applications.  From now on, Bluez acts as a
middleware between an audio application, which implements Bluetooth audio
profile, and a Bluetooth audio device.  BlueALSA registers all known Bluetooth
audio profiles in Bluez, so in theory every Bluetooth device (with audio
capabilities) can be connected.  In order to access the audio stream, one has
to connect to the ALSA PCM device called @code{bluealsa}.  The device is based
on the ALSA software PCM plugin.")
    (license license:expat)))

(define-public snd
  (package
    (name "snd")
    (version "20.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ccrma-ftp.stanford.edu/pub/Lisp/"
                                  "snd-" version ".tar.gz"))
              (sha256
               (base32
                "0jxkycxn6jcbs4gklk9sk3gfr0y26dz1m71nxah9rnx80wnzj6hr"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:out-of-source? #f              ; for the 'install-doc' phase
       #:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (docdir (string-append out "/share/doc/"
                                     ,name "-" ,version)))
         (list "--with-alsa" "--with-jack" "--with-gmp"
               (string-append "--with-doc-dir=" docdir)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/"
                                        ,name "-" ,version)))
               (for-each
                (lambda (f)
                  (install-file f doc))
                (find-files "." "\\.html$"))
               (copy-recursively "pix" (string-append doc "/pix"))
               #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           fftw
           flac
           gmp
           gsl
           gtk+
           jack-1
           libsamplerate
           mpc
           mpfr
           mpg123
           speex
           timidity++
           vorbis-tools
           wavpack))
    (synopsis "Sound editor")
    (home-page "https://ccrma.stanford.edu/software/snd/")
    (description
     "Snd is a sound editor modelled loosely after Emacs.  It can be
customized and extended using either the s7 Scheme implementation (included in
the Snd sources), Ruby, or Forth.")
    (license (license:non-copyleft "file://COPYING"))))

(define-public noise-repellent
  (package
    (name "noise-repellent")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lucianodato/noise-repellent")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0hb89x9i2knzan46q4nwscf5zmnb2nwf4w13xl2c0y1mx1ls1mwl"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--prefix=" (assoc-ref %outputs "out")
                            "/lib/lv2"))))
    (inputs
     (list lv2 fftwf))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/lucianodato/noise-repellent")
    (synopsis "LV2 plugin for broadband noise reduction")
    (description "Noise Repellent is an LV2 plugin to reduce noise.  It has
the following features:

@enumerate
@item Spectral gating and spectral subtraction suppression rule
@item Adaptive and manual noise thresholds estimation
@item Adjustable noise floor
@item Adjustable offset of thresholds to perform over-subtraction
@item Time smoothing and a masking estimation to reduce artifacts
@item Basic onset detector to avoid transients suppression
@item Whitening of the noise floor to mask artifacts and to recover higher
  frequencies
@item Option to listen to the residual signal
@item Soft bypass
@item Noise profile saved with the session
@end enumerate
")
    (license license:lgpl3+)))

(define-public lv2-speech-denoiser
  (let ((commit "04cfba929630404f8d4f4ca5bac8d9b09a99152f")
        (revision "1"))
    (package
      (name "lv2-speech-denoiser")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lucianodato/speech-denoiser/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "189l6lz8sz5vr6bjyzgcsrvksl1w6crqsg0q65r94b5yjsmjnpr4"))))
      (build-system meson-build-system)
      (arguments
       `(;; Using a "release" build is recommended for performance
         #:build-type "release"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-meson-build
             (lambda _
               (substitute* "meson.build"
                 (("install_folder = 'sdenoise.lv2'")
                  "install_folder = 'lib/lv2/sdenoise.lv2'")
                 (("build/manifest.ttl") "../build/manifest.ttl"))
               #t))
           (add-after 'unpack 'build-rnnoise
             (lambda _
               (with-directory-excursion "rnnoise"
                 (let ((old-CFLAGS (getenv "CFLAGS")))
                   (setenv "CFLAGS" "-fvisibility=hidden -fPIC -Wl,--exclude-libs,ALL")
                   (setenv "CONFIG_SHELL" (which "bash"))
                   (invoke "autoreconf" "-vif")
                   (invoke "sh" "configure"
                           "--disable-examples"
                           "--disable-doc"
                           "--disable-shared"
                           "--enable-static")
                   (invoke "make")
                   (setenv "CFLAGS" old-CFLAGS))))))))
      (inputs
       (list lv2))
      (native-inputs
       (list autoconf automake libtool pkg-config))
      (home-page "https://github.com/werman/noise-suppression-for-voice")
      (synopsis "Speech denoise LV2 plugin based on Xiph's RNNoise library")
      (description "RNNoise is a library that uses deep learning to apply
noise suppression to audio sources with voice presence.  This package provides
an LV2 audio plugin.")
      (license license:lgpl3+))))

(define-public cli-visualizer
  (package
    (name "cli-visualizer")
    (version "1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dpayne/cli-visualizer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "003mbbwsz43mg3d7llphpypqa9g7rs1p1cdbqi1mbc2bfrc1gcq2"))))
    (build-system cmake-build-system)
    (native-inputs
     ;; TODO: Try using the latest googletest for versions > 1.8.
     (list ;; ("googletest" ,googletest-1.8)
           which))
    (inputs
     (list fftw ncurses pulseaudio))
    (arguments
     '(#:tests? #f
       ;; XXX Enable tests after patching them to use the system googletest.
       ;; #:configure-flags (list "-DVIS_WITH_TESTS=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-examples
           (lambda* (#:key outputs #:allow-other-keys)
             (with-directory-excursion "../source/examples"
               (delete-file "mac_osx_config")
               (for-each (lambda (file)
                           (install-file file
                                         (string-append
                                          (assoc-ref outputs "out")
                                          "/share/doc")))
                         (find-files ".")))
             #t)))))
    (home-page "https://github.com/dpayne/cli-visualizer/")
    (synopsis "Command-line audio visualizer")
    (description "@code{cli-visualizer} displays fast-Fourier
transforms (FFTs) of the sound being played, as well as other graphical
representations.")
    (license license:expat)))

(define-public cava
  (package
    (name "cava")
    (version "0.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/karlstav/cava")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mziklmqifhnb4kg9ia2r56r8wjn6xp40bkpf484hsgqvnrccl86"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list fftw ncurses pulseaudio))
    (arguments
     `(#:configure-flags
       (list (string-append "PREFIX=" %output)
             (string-append "FONT_DIR=" %output "/share/consolefonts"))
       #:make-flags
       (let ((lib (string-append %output "/lib")))
         (list (string-append "cava_LDFLAGS = -L" lib " -Wl,-rpath " lib)))
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "HOME" (getcwd))
             (invoke "sh" "autogen.sh")))
         (add-before 'build 'make-cava-ldflags
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib"))
             #t))
         (add-after 'install 'data
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each (lambda (file)
                         (install-file file
                                       (string-append (assoc-ref outputs "out")
                                                      "/share/doc/examples")))
                       (find-files "example_files"))
             #t)))))
    (home-page "https://karlstav.github.io/cava/")
    (synopsis "Console audio visualizer for ALSA, MPD, and PulseAudio")
    (description "C.A.V.A. is a bar audio spectrum visualizer for the terminal
using ALSA, MPD, PulseAudio, or a FIFO buffer as its input.")
    (license license:expat)))

(define-public fluid-3
  (let ((commit "871c8ce2002e8b3c198f532fdb4fbcce7914f951"))
    (package
      (name "fluid-3")
      (version "2.1")
      (source
       (origin
         (method url-fetch)
         ;; Only one file is required, but the release bundles the whole
         ;; software which is 50MiB as tar and 200MiB unpacked. The website
         ;; directly links the soundfont release to the github file download.
         (uri (string-append "https://github.com/musescore/MuseScore/raw/"
                             commit "/share/sound/FluidR3Mono_GM.sf3"))
         (file-name (string-append name "-" version ".sf3"))
         (sha256
          (base32
           "1hjfg5i15bw9279007xs92zsggjgn4s4k9pc00s851l3kvc6dkfg"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let ((file (assoc-ref %build-inputs "source"))
                 (out (string-append %output "/share/soundfonts")))
             (mkdir-p out)
             (copy-file file (string-append out "/FluidR3Mono_GM.sf3"))
             #t))))
      (home-page  "https://github.com/musescore/MuseScore/tree/master/share/sound")
      (synopsis "Pro-quality GM soundfont")
      (description "Fluid-3 is Frank Wen's pro-quality GM soundfont.")
      (license license:expat))))

(define-public libfdk
  (package
    (name "libfdk")
    (version "2.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mstorsjo/fdk-aac")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1fkrnzs78fmj11n9z3l0w53i2fl16jcfiyavwidck9bzmkmsf486"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://github.com/mstorsjo/fdk-aac")
    (synopsis "Fraunhofer FDK AAC library")
    (description "FDK is a library for encoding and decoding Advanced Audio
Coding (AAC) format audio, developed by Fraunhofer IIS, and included as part of
Android.  It supports several Audio Object Types including MPEG-2 and MPEG-4 AAC
LC, HE-AAC (AAC LC + SBR), HE-AACv2 (LC + SBR + PS) as well AAC-LD (low delay)
and AAC-ELD (enhanced low delay) for real-time communication.  The encoding
library supports sample rates up to 96 kHz and up to eight channels (7.1
                                                                     surround).")
    (license (license:fsf-free "https://github.com/mstorsjo/fdk-aac/blob/master/NOTICE"
                               "https://www.gnu.org/licenses/license-list.html#fdk"))))

(define-public libopenshot-audio
  (package
    (name "libopenshot-audio")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenShot/libopenshot-audio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03dygh85riljk7dpn5a5a0d22a2kz45fs13gzwqgnbzzr1k17p2y"))))
    (build-system cmake-build-system)
    (inputs
     (list alsa-lib
           ;; The following are for JUCE GUI components:
           libx11
           freetype
           libxrandr
           libxinerama
           libxcursor))
    (arguments
     `(#:tests? #f                      ;there are no tests
       #:configure-flags
       (list (string-append "-DCMAKE_CXX_FLAGS=-I"
                            (assoc-ref %build-inputs "freetype")
                            "/include/freetype2"))))
    (home-page "https://openshot.org")
    (synopsis "Audio editing and playback for OpenShot")
    (description "OpenShot Audio Library (libopenshot-audio) allows
high-quality editing and playback of audio, and is based on the JUCE
library.")
    (license license:lgpl3+)))

(define-public faudio
  (package
    (name "faudio")
    (version "21.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FNA-XNA/FAudio")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l9bicg8v1shsyq9k48zh4wv5kwfs6lfjmm9blzd13xrgmhd07w2"))))
    (arguments
     '(#:tests? #f                      ; No tests.
       #:configure-flags '("-DGSTREAMER=ON")))
    (build-system cmake-build-system)
    (native-inputs (list pkg-config))
    (inputs (list gstreamer gst-plugins-base sdl2))
    (home-page "https://github.com/FNA-XNA/FAudio")
    (synopsis "XAudio reimplementation")
    (description "FAudio is an XAudio reimplementation that focuses solely on
developing fully accurate DirectX Audio runtime libraries.")
    (license
     (list license:zlib
           ;; stb & utils/{ui,wav}common are dual-licenced under either of:
           license:expat
           license:public-domain))))

(define-public gnaural
  (package
    (name "gnaural")
    (version "20110606")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gnaural/Gnaural/gnaural_"
                           version ".tar.xz"))
       (sha256
        (base32
         "1gq519c0imsh57zklyi0f8h64l3ai48lh672c834470z8c6kvbfi"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib gtk+-2 libsndfile portaudio))
    (native-inputs
     (list pkg-config))
    (home-page "http://gnaural.sourceforge.net/")
    (synopsis "Binaural beat synthesizer")
    (description "Gnaural is a programmable auditory binaural beat synthesizer
intended to be used for brainwave entrainment.  Gnaural supports creation of
binaural beat tracks of different frequencies and exporting of tracks into
different audio formats.  Gnaural can also be linked over the internet with
other Gnaural instances, allowing synchronous sessions between many users.")
    (license license:gpl2+)))

(define-public darkice
  (package
    (name "darkice")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rafael2k/darkice/releases/"
                                  "download/v" version "/darkice-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "05yq7lggxygrkd76yiqby3msrgdn082p0qlvmzzv9xbw8hmyra76"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list lame
                  libvorbis
                  opus
                  twolame
                  alsa-lib
                  pulseaudio
                  jack-1
                  libsamplerate))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-lame-prefix="
                            (assoc-ref %build-inputs "lame")))))
    (home-page "http://www.darkice.org/")
    (synopsis "Live audio streamer")
    (description "DarkIce is a live audio streamer.  It takes audio input from
a sound card, encodes it into Ogg Vorbis and/or mp3, and sends the audio
stream to one or more IceCast and/or ShoutCast servers.")
    (license license:gpl3+)))

(define-public libltc
  (package
    (name "libltc")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/x42/libltc/releases/download/v"
                       version "/libltc-" version ".tar.gz"))
       (sha256
        (base32
         "173h9dgmain3nyrwk6q2d7yl4fnh4vacag4s2p01n5b7nyrkxrjh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-tests? #f))           ;tests fail otherwise
    (native-inputs
     (list doxygen pkg-config))
    (synopsis "Encode or decode Linear/Longitudinal Time Code (LTC) audio")
    (description "Libltc is a POSIX-C Library for handling
@dfn{Linear/Longitudinal Time Code} (LTC) data.")
    (home-page "https://x42.github.io/libltc/")
    (license license:lgpl3+)))

(define-public ttaenc
  (package
    (name "ttaenc")
    (version "3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tta/"
                           "tta/ttaenc-src"
                           "/ttaenc-" version "-src.tgz"))
       (sha256
        (base32
         "1iixpr4b89g9g1hwn8ak8k8iflcww3r5f09a117qdidc2nqcijdj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "INSDIR=" (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure
         (add-before 'install 'make-bindir
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               #t))))))
    (synopsis "TTA lossless audio encoder")
    (description
     "TTA performs lossless compression on multichannel 8,16 and 24 bits
data of the Wav audio files.  Being lossless means that no data-
quality is lost in the compression - when uncompressed, the data will
be identical to the original.  The compression ratios of TTA depend on
the type of music file being compressed, but the compression size
will generally range between 30% - 70% of the original.  TTA format
supports both of ID3v1/v2 and APEv2 tags.")
    (home-page "http://tausoft.org/")
    (license license:gpl2+)))

(define-public libsoundio
  (package
    (name "libsoundio")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andrewrk/libsoundio")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "12l4rvaypv87vigdrmjz48d4d6sq4gfxf5asvnc4adyabxb73i4x"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ;no tests included
    (inputs
     (list alsa-lib jack-1 pulseaudio))
    (native-inputs
     (list pkg-config))
    (home-page "http://libsound.io")
    (synopsis "C library for real-time audio input and output")
    (description "@code{libsoundio} is a C library providing audio input and
output.  The API is suitable for real-time software such as digital audio
workstations as well as consumer software such as music players.")
    (license license:expat)))

(define-public redkite
  (package
    (name "redkite")
    (version "1.3.1")                     ;marked unmaintained as of Oct. 2021
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/free-sm/redkite")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zb2k2a4m7z2ravqrjn8fq8lic20wbr2m8kja3p3113jsk7j9zvd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no tests included
    (propagated-inputs
     (list cairo))
    (native-inputs
     (list pkg-config))
    (synopsis "Small GUI toolkit")
    (description "Redkite is a small GUI toolkit developed in C++17 and
inspired from other well known GUI toolkits such as Qt and GTK.  It is
minimal on purpose and is intended to be statically linked to applications,
therefore satisfying any requirements they may have to be self contained,
as is the case with audio plugins.")
    (home-page "https://gitlab.com/geontime/redkite")
    (license license:gpl3+)))

(define-public carla
  (package
    (name "carla")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/falkTX/Carla")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01ngkmfcxyg1bb4qmfvlkkjbx4lx62akxqhizl8zmqnhfcy4p9bx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'build 'set-CC-variable-and-show-features
           (lambda _
             (setenv "CC" "gcc")
             (invoke "make" "features")))
         (add-after 'install 'make-carla-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (chmod (string-append out "/share/carla/carla") #o555)
               #t)))
         (add-after 'install 'wrap-executables
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-script (string-append out "/bin/carla")
                            #:guile (search-input-file inputs "bin/guile")
                            `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH"))))
               #t))))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ffmpeg" ,ffmpeg)
       ("fluidsynth" ,fluidsynth)
       ("file" ,file)
       ("liblo" ,liblo)
       ("libsndfile" ,libsndfile)
       ("gtk2" ,gtk+-2)   ;needed for bridging GTK2 plugins in GTK3 hosts
       ("gtk+" ,gtk+)
       ("python-pyliblo" ,python-pyliblo)
       ("python-pyqt" ,python-pyqt)
       ("python-rdflib" ,python-rdflib)
       ;; python-pyqt shows the following error without python-wrapper:
       ;; Error while finding module specification for 'PyQt5.uic.pyuic'
       ;; (ModuleNotFoundError: No module named 'PyQt5')
       ("python-wrapper" ,python-wrapper)
       ("libx11" ,libx11)
       ("qtbase" ,qtbase-5)
       ("zlib" ,zlib)

       ;; For WRAP-SCRIPT above.
       ("guile" ,guile-2.2)))
    (native-inputs
     (list pkg-config))
    (home-page "https://kx.studio/Applications:Carla")
    (synopsis "Audio plugin host")
    (description "Carla is a modular audio plugin host, with features like
transport control, automation of parameters via MIDI CC and remote control
over OSC.  Carla currently supports LADSPA (including LRDF), DSSI, LV2, VST2,
and VST3 plugin formats, plus SF2 and SFZ file support.  It uses JACK as the
default and preferred audio driver but also supports native drivers like ALSA.")
    (license license:gpl2+)))

(define-public ecasound
  (package
    (name "ecasound")
    (version "2.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nosignal.fi/download/ecasound-"
                                  version ".tar.gz"))
              (sha256
               (base32 "1m7njfjdb7sqf0lhgc4swihgdr4snkg8v02wcly08wb5ar2fr2s6"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    ;; It would be nice to add mikmod to inputs if that gets packaged
    ;; eventually.
    (inputs (list alsa-lib
                  jack-1
                  mpg123
                  lame
                  vorbis-tools
                  faad2
                  flac
                  timidity++
                  libsndfile
                  libsamplerate
                  ncurses
                  ladspa
                  lilv))
    (home-page "https://nosignal.fi/ecasound/index.php")
    (synopsis "Multitrack audio processing")
    (description "Ecasound is a software package designed for multitrack audio
processing. It can be used for simple tasks like audio playback, recording and
format conversions, as well as for multitrack effect processing, mixing,
recording and signal recycling. Ecasound supports a wide range of audio inputs,
outputs and effect algorithms. Effects and audio objects can be combined in
various ways, and their parameters can be controlled by operator objects like
oscillators and MIDI-CCs. A versatile console mode user-interface is included
in the package.")
    ;; As an exception to the above, the C, C++ and python implementations
    ;; of the Ecasound Control Interface (ECI) are licensed under the LGPL
    ;; (see the file 'COPYING.LGPL'). This allows writing ECI applications
    ;; that are not licensed under GPL.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public libaudec
  (package
    (name "libaudec")
    (version "0.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.zrythm.org/zrythm/libaudec")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1570m2dfia17dbkhd2qhx8jjihrpm7g8nnyg6n4wif4vv229s7dz"))))
   (build-system meson-build-system)
   (arguments
    `(#:configure-flags
      ;; Build the tests.
      `("-Dtests=true")))
   (inputs
    (list libsamplerate libsndfile))
   (native-inputs
     (list pkg-config))
   (synopsis "Library for reading and resampling audio files")
   (description "libaudec is a wrapper library over ffmpeg, sndfile and
libsamplerate for reading and resampling audio files, based on Robin Gareus'
@code{audio_decoder} code.")
   (home-page "https://git.zrythm.org/zrythm/libaudec")
   (license license:agpl3+)))

(define-public lv2lint
  (package
    (name "lv2lint")
    (version "0.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.open-music-kontrollers.ch/lv2/lv2lint")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1jrka0hsn4n1clri7zfkcl3c2vi52144lkpjm81l51ff8rqy8ks1"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       `("-Delf-tests=true" ; for checking symbol visibility
         "-Donline-tests=true"))) ; for checking URI existence
    (inputs
      (list curl libelf lilv))
    (native-inputs
      (list pkg-config))
    (synopsis "LV2 plugin lint tool")
    (description "lv2lint is an LV2 lint-like tool that checks whether a
given plugin and its UI(s) match up with the provided metadata and adhere
to well-known best practices.")
    (home-page "https://open-music-kontrollers.ch/lv2/lv2lint/")
    (license license:artistic2.0)))

(define-public lv2toweb
  (package
    (name "lv2toweb")
    (version "0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/x42/lv2toweb")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "007aysqvgkf25za8nkmyd5g9kp1zla460dcpidlj5xg1zc3fcdfi"))))
    (build-system gnu-build-system)
    (arguments
    `(#:tests? #f  ; no "check" target
      #:make-flags (list "CC=gcc"
                         (string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
    (inputs
      (list jalv lilv))
    (native-inputs
      (list help2man pkg-config))
    (synopsis "Documentation generator for LV2 plugins")
    (description
      "lv2toweb allows the user to create an xhtml page with information
about the given LV2 plugin, provided that the plugin and its UI(s) match up
with the provided metadata and adhere to well-known best practices.")
    (home-page "https://github.com/x42/lv2toweb")
    (license (list license:isc license:gpl2))))

(define-public ztoolkit
  (package
    (name "ztoolkit")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.zrythm.org/zrythm/ztoolkit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07xl3cmdaf7k9mm58m93cn8i1jvgimmiifdw1w7v2jl88nx60pm1"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    ;; These are listed as propagated inputs because they are dependencies
    ;; in pkgconfig.
    (propagated-inputs
     (list cairo libx11))
    (synopsis "GUI toolkit for LV2 plugins")
    (description "ZToolkit (Ztk) is a cross-platform GUI toolkit heavily
inspired by GTK.  It handles events and low level drawing on behalf of
the user and provides a high-level API for managing the UI and custom
widgets.  ZToolkit is written in C and was created to be used for building
audio plugin UIs, where the dependencies often need to be kept to a
minimum.")
    (home-page "https://git.zrythm.org/zrythm/ztoolkit")
    (license license:agpl3+)))

(define-public libinstpatch
  (package
    (name "libinstpatch")
    (version "1.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/swami/libinstpatch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w3nk0vvd1cxic70n45zjip0bdsrja969myvyvkhq3ngbarbykir"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ;there are no tests
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     (list glib libsndfile))
    (home-page "http://www.swamiproject.org/")
    (synopsis "Instrument file software library")
    (description
     "libInstPatch is a library for processing digital sample based MIDI
instrument \"patch\" files.  The types of files libInstPatch supports are used
for creating instrument sounds for wavetable synthesis.  libInstPatch provides
an object framework (based on GObject) to load patch files, which can then be
edited, converted, compressed and saved.")
    (license license:lgpl2.1)))

(define-public ztoolkit-rsvg
  (package/inherit ztoolkit
    (name "ztoolkit-rsvg")
    (arguments
     `(#:configure-flags `("-Denable_rsvg=true")))
    (propagated-inputs
     `(("librsvg" ,librsvg)
       ,@(package-propagated-inputs ztoolkit)))
    (synopsis "ZToolkit with SVG support")))

(define-public lsp-dsp-lib
  (package
    (name "lsp-dsp-lib")
    (version "0.5.14")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sadko4u/lsp-dsp-lib/"
                            "releases/download/" version
                            "/lsp-dsp-lib-" version "-src.tar.gz"))
        (sha256
         (base32 "1gcznkyybywbgdi2fhx27i8sckhy6ahvxax72b213g1lr5aaw7bq"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no tests
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target)))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'omit-static-library
                 (lambda _
                   (substitute* "src/Makefile"
                     ((".*cp \\$\\(ARTIFACT_SLIB\\).*") "") ; don't install it
                     ((" \\$\\(ARTIFACT_SLIB\\)") ""))))    ; don't build it
               (replace 'configure
                 (lambda _
                   (invoke "make" "config"
                           (string-append "PREFIX=" #$output)))))))
    (home-page "https://github.com/sadko4u/lsp-dsp-lib")
    (synopsis "Digital signal processing library")
    (description "The LSP DSP library provides a set of functions that perform
SIMD-optimized computing on several hardware architectures.  All functions
currently operate on IEEE-754 single-precision floating-point numbers.")
    (license license:lgpl3+)))

(define-public codec2
  (package
    (name "codec2")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/drowe67/codec2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jpvr7bra8srz8jvnlbmhf8andbaavq5v01qjnp2f61za93rzwba"))))
    (build-system cmake-build-system)
    (native-inputs
     (list bc octave valgrind))
    (arguments
     `(#:tests? #f ; TODO: Fix tests (paths, graphic toolkit, octave modules).
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-test-environment
           (lambda _
             (setenv "HOME" "/tmp")
             #t)))))
    (synopsis "Speech codec")
    (description
     "Codec 2 is a speech codec designed for communications quality speech
between 700 and 3200 bit/s.  The main application is low bandwidth HF/VHF
digital radio.")
    (home-page "https://www.rowetel.com/?page_id=452")
    (license license:lgpl2.1)))

(define-public mbelib
  ;; No release since 2016, use commit directly.
  (let ((commit "9a04ed5c78176a9965f3d43f7aa1b1f5330e771f")
        (revision "1"))
    (package
      (name "mbelib")
      (version (git-version "1.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/szechyjs/mbelib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0a7xmf87xnjzm5b437j2vnwv39x0ascja1j04c5wj6xs1529gw8h"))))
      (build-system cmake-build-system)
      (home-page "https://github.com/szechyjs/mbelib")
      (synopsis "P25 Phase 1 and ProVoice vocoder")
      (description
       "The mbelib library provides support for the 7200x4400 bit/s codec used
in P25 Phase 1, the 7100x4400 bit/s codec used in ProVoice and the @emph{Half
Rate} 3600x2250 bit/s vocoder used in various radio systems.")
      (license (list license:bsd-3      ; test/ framework
                     license:isc)))))   ; the rest

(define-public ableton-link
  (package
    (name "ableton-link")
    (version "3.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Ableton/link")
                    (commit (string-append "Link-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wplqj11ww64gmw2kzlxpvfs3v04m2036f7k5ndm34zcv12b91fa"))
              (modules '((guix build utils)))
              (patches
               (search-patches "ableton-link-system-libraries-debian.patch"))
              (snippet
               '(begin
                  ;; Tests assume that CMake's "build" directory is a
                  ;; sub-directory of the source tree, so we fix it.
                  (substitute* "ci/run-tests.py"
                    (("root_dir,") "root_dir, os.pardir,"))
                  ;; Unbundle dependencies.
                  (delete-file-recursively "third_party")
                  (delete-file-recursively "modules")))))
    (build-system cmake-build-system)
    (native-inputs
     (list catch-framework
           python ;for running tests
           portaudio ;for portaudio examples
           qtbase-5 ;for Qt examples
           qtdeclarative
           qttools))
    (inputs
     (list jack-1 ;for JACK examples
           qtquickcontrols)) ;for Qt examples
    (propagated-inputs
     ;; This is because include/ableton/platforms/asio/AsioWrapper.hpp
     ;; contains '#include <asio.hpp>'.
     (list asio))
    (arguments
     `(#:configure-flags
       '("-DLINK_BUILD_QT_EXAMPLES=ON"
         "-DLINK_BUILD_JACK=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs tests? #:allow-other-keys)
             (when tests?
               (let* ((python (search-input-file inputs "/bin/python3"))
                      (run-tests "../source/ci/run-tests.py"))
                 (invoke python run-tests "--target" "LinkCoreTest")
                 (invoke python run-tests "--target" "LinkDiscoveryTest")))))
         (add-before 'install 'patch-cmake
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((source "../source/"))
               (substitute* (string-append source
                                           "cmake_include/AsioStandaloneConfig.cmake")
                 (((string-append "\\$\\{CMAKE_CURRENT_LIST_DIR\\}/\\.\\./"
                                  "modules/asio-standalone/asio/include"))
                  (string-append (assoc-ref inputs "asio")
                                 "/include")))
               (substitute* (string-append source "AbletonLinkConfig.cmake")
                 (("\\$\\{CMAKE_CURRENT_LIST_DIR\\}/include")
                  "${CMAKE_CURRENT_LIST_DIR}/../../../include")
                 (("\\$\\{CMAKE_CURRENT_LIST_DIR\\}/include/ableton/Link\\.hpp")
                  "${CMAKE_CURRENT_LIST_DIR}/../../../include/ableton/Link.hpp")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib-cmake (string-append out "/lib/cmake/ableton-link"))
                    (source "../source"))
               (for-each (lambda (test-file)
                           (delete-file test-file))
                         '("bin/LinkDiscoveryTest" "bin/LinkCoreTest"))
               (copy-recursively "bin" bin)
               (copy-recursively (string-append source "/include/ableton")
                                 (string-append out "/include/ableton"))
               (install-file (string-append source "/AbletonLinkConfig.cmake")
                             lib-cmake)
               (install-file (string-append source
                                            "/cmake_include/AsioStandaloneConfig.cmake")
                             (string-append lib-cmake "/cmake_include"))))))))
    (home-page "https://github.com/Ableton/link")
    (synopsis "Synchronize musical beat, tempo, and phase across multiple applications")
    (description
     "Ableton Link is a C++ library that synchronizes musical beat, tempo, and phase
across multiple applications running on one or more devices.  Applications on devices
connected to a local network discover each other automatically and form a musical
session in which each participant can perform independently: anyone can start or stop
while still staying in time.")
    (license license:gpl2+)))

(define-public butt
  (package
    (name "butt")
    (version "0.1.32")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/butt/butt/butt-"
                                  version "/butt-" version ".tar.gz"))
              (sha256
               (base32
                "1qwllkx9p1gb3syhbbck3agrk375m82l18fb81aqygi4g3dg3s9r"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "src/butt.cpp"
                  ((".*zica.*") "")))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-documentation
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (manual (assoc-ref inputs "manual"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "README" doc)
               (copy-file manual (string-append doc "/butt-manual.pdf"))))))))
    (inputs
     `(("dbus" ,dbus)
       ("flac" ,flac)
       ("fltk" ,fltk)
       ("lame" ,lame)
       ("libfdk" ,libfdk)
       ("libsamplerate" ,libsamplerate)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxft" ,libxft)
       ("libxrender" ,libxrender)
       ("ogg" ,libogg)
       ("openssl" ,openssl)
       ("opus" ,opus)
       ("portaudio" ,portaudio)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("manual" ,(origin
                    (method url-fetch)
                    (uri (string-append "https://danielnoethen.de/butt/butt-"
                                        version "_manual.pdf"))
                    (sha256
                     (base32
                      "0g70jyyxbx5nin3xs9q9zf878b2kyy7rn8gn9w91x1ychbjd6dhh"))))))
    (home-page "https://danielnoethen.de/butt/")
    (synopsis "Audio streaming tool")
    (description "Butt is a tool to stream audio to a ShoutCast or
Icecast server.")
    (license license:gpl2+)))

(define-public siggen
  (package
    (name "siggen")
    (version "2.3.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bleskodev/siggen")
             (commit "a407611b59d59c7770bbe62ba9b8e9a948cf3210")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0szhgfd9kddr6qsz0imp0x66jjn6ry236f35vjl82ivc1v2bllcb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "INSDIR=" %output "/bin")
                          (string-append "MANDIR=" %output "/share/man"))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         ;; Patch misc.c to prevent a segfault.
         (add-after 'unpack 'patch-segfault
           (lambda _
             (substitute* "misc.c"
               (("#include <stdio.h>\n" all)
                (string-append all "#include <string.h>\n")))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key make-flags outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (dir)
                           (mkdir-p (string-append out dir)))
                         (list "/bin" "/share/man/man1" "/share/man/man5"))
               (apply invoke "make" "sysinstall" make-flags)))))))
    (inputs
     (list ncurses))
    (native-inputs
     `(("groff" ,groff-minimal)         ; for nroff
       ("util-linux" ,util-linux)))     ; for col
    (home-page "https://github.com/bleskodev/siggen")
    (synopsis "Signal generation tools")
    (description "siggen is a set of tools for imitating a laboratory signal
generator, generating audio signals out of Linux's /dev/dsp audio
device.  There is support for mono and/or stereo and 8 or 16 bit samples.")
    (license license:gpl2)))

(define-public python-pysox
  ;; PyPi does not include the data folder containing audio files for testing.
  (let ((commit "3d0053381c24ae3490f759d4de87194b85789d36")
        (revision "0"))
    (package
      (name "python-pysox")
      (version (git-version "1.4.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rabitt/pysox")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0i62jx92vfpcr2z7lp69yzqdi9idfs3pifl3rzm2akc2c4cr1mac"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-sox
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((sox-store-path (assoc-ref inputs "sox"))
                      (sox-bin (string-append sox-store-path "/bin/sox")))
                 (substitute* "sox/__init__.py"
                   (("sox -h")
                    (string-append sox-bin " -h")))
                 (substitute* "sox/core.py"
                   (("\\['sox")
                    (string-append "['" sox-bin))))))
           (replace 'check
             (lambda* (#:key inputs outputs tests? #:allow-other-keys)
               (when tests?
                 (add-installed-pythonpath inputs outputs)
                 (invoke "pytest")))))))
      (propagated-inputs
       (list python-numpy python-typing-extensions))
      (native-inputs
       (list sox python-pytest python-pytest-cov python-soundfile))
      (home-page "https://github.com/rabitt/pysox")
      (synopsis "Python wrapper around SoX")
      (description "@code{python-pysox} is a wrapper around the @command{sox}
command line tool.  The API offers @code{Transformer} and @code{Combiner}
classes that allow the user to incrementally build up effects and audio
manipulations.  @code{python-pysox} also provides methods for querying audio
information such as sample rate, determining whether an audio file is silent,
and much more.")
      (license license:bsd-3))))

(define-public python-resampy
  (package
    (name "python-resampy")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         ;; PyPi does not include tests.
         (url "https://github.com/bmcfee/resampy")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qmkxl5sbgh0j73n667vyi7ywzh09iaync91yp1j5rrcmwsn0qfs"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "tests")))))))
    (propagated-inputs
     (list python-numba python-numpy python-scipy python-six))
    (native-inputs
     (list python-pytest python-pytest-cov))
    (home-page "https://github.com/bmcfee/resampy")
    (synopsis "Efficient signal resampling")
    (description
     "@code{python-resampy} implements the band-limited sinc interpolation
method for sampling rate conversion as described by Julius O. Smith at the
@url{https://ccrma.stanford.edu/~jos/resample/, Digital Audio Resampling
Home Page}.")
    (license license:isc)))

(define-public python-librosa
  (package
    (name "python-librosa")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "librosa" version))
       (sha256
        (base32 "1cx6rhcvak0hy6bx84jwzpxmwgi92m82w77279akwjmfd3khagf5"))))
    (build-system python-build-system)
    (arguments
     ;; Tests require internet connection to download MATLAB scripts for
     ;; generating the testing data.
     `(#:tests? #f))
    (propagated-inputs
     (list python-audioread
           python-decorator
           python-joblib
           python-numba
           python-numpy
           python-packaging
           python-pooch
           python-resampy
           python-scikit-learn
           python-scipy
           python-soundfile))
    (home-page "https://librosa.org")
    (synopsis "Python module for audio and music processing")
    (description
     "@code{librosa} is a python package for music and audio analysis.  It
provides the building blocks necessary to create music information retrieval
systems.")
    (license license:isc)))

(define-public mda-lv2
  (package
    (name "mda-lv2")
    (version "1.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://download.drobilla.net/mda-lv2-"
                            version ".tar.bz2"))
        (sha256
         (base32 "1nspk2j11l65m5r9z5isw8j749vh9a89wgx8mkrrq15f4iq12rnd"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f  ; There are no tests.
       #:configure-flags
       (list (string-append "--prefix="
                            (assoc-ref %outputs "out")))))
    (inputs
     (list lv2))
    (native-inputs
     (list pkg-config))
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))
    (home-page "https://drobilla.net/software/mda-lv2")
    (synopsis "Audio plug-in pack for LV2")
    (description
     "MDA-LV2 is an LV2 port of the MDA plugins.  It includes effects and a few
instrument plugins.")
    (license license:gpl3+)))

(define-public libodiosacd
  (package
   (name "libodiosacd")
   (version "21.8.30")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/tari01/libodiosacd")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0iamf7wksbql0qfigdv5ahaax53ms2yligdav8dw6x0ay88x4lhi"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f ; no check target
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-makefile
          (lambda _
            (substitute* "Makefile"
              (("\\$\\(DESTDIR\\)/usr")
                "\\$(DESTDIR)"))))
        (delete 'configure)) ; no configure script
      #:make-flags
      (list (string-append "DESTDIR=" %output))))
   (synopsis "Library for decoding Super Audio CDs (SACD)")
   (description
    "The Odio SACD shared library is a decoding engine which takes a Super
Audio CD source and extracts a 24-bit high resolution WAV file.  It handles
both DST and DSD streams.")
   (home-page "https://tari.in/www/software/libodiosacd/")
   (license license:gpl3+)))

(define-public odio-sacd
  (package
   (name "odio-sacd")
   (version "21.1.9")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/tari01/odio-sacd")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0314srqk0r4qv292qiaply619l2fw04nkdwvqhj3q1dqzv41g4qk"))))
   (inputs (list libodiosacd))
   ;; Build system and arguments for libodiosacd are identical.
   (build-system (package-build-system libodiosacd))
   (arguments (package-arguments libodiosacd))
   (synopsis "Rip Super Audio CDs (SACD)")
   (description
    "Odio SACD is a command-line application which takes a Super Audio CD
source and extracts a 24-bit high resolution WAV file.  It handles both DST
and DSD streams.")
   (home-page "https://tari.in/www/software/odio-sacd/")
   (license license:gpl3+)))
