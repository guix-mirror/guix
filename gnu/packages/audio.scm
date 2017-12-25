;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016, 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system waf)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet) ; libmicrohttpd
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages mp3) ;taglib
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)  ;libsndfile, libsamplerate
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim) ;xxd
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public alsa-modular-synth
  (package
    (name "alsa-modular-synth")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/alsamodular/alsamodular"
                                  "/" version "/ams-" version ".tar.bz2"))
              (sha256
               (base32
                "1azbrhpfk4nnybr7kgmc7w6al6xnzppg853vas8gmkh185kk11l0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--enable-qt5"
         "CXXFLAGS=-std=gnu++11")
       #:phases
       (modify-phases %standard-phases
         ;; Insert an extra space between linker flags.
         (add-before 'configure 'add-missing-space
           (lambda _
             (substitute* "configure"
               (("LIBS\\+=\\$LIBSsave") "LIBS+=\" $LIBSsave\"")
               (("CFLAGS\\+=\\$CFLAGSsave") "CFLAGS+=\" $CFLAGSsave\""))
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ;; We cannot use zita-alsa-pcmi (the successor of clalsadrv) due to
       ;; license incompatibility.
       ("clalsadrv" ,clalsadrv)
       ("fftw" ,fftw)
       ("jack" ,jack-1)
       ("ladspa" ,ladspa)
       ("liblo" ,liblo)
       ("qtbase" ,qtbase)
       ("qttools" ,qttools)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (version "0.4.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://aubio.org/pub/aubio-" version ".tar.bz2"))
             (sha256
              (base32
               "15f6nf76y7iyl2kl4ny7ky0zpxfxr8j3902afvd6ydnnkh5dzmr5"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f  ; no check target
       #:configure-flags
       '("--enable-fftw3f"
         "--enable-jack"
         "--enable-sndfile"
         "--enable-samplerate"
         ;; enable compilation with avcodec once available
         "--disable-avcodec")
       #:python ,python-2))
    (inputs
     `(("jack" ,jack-1)
       ("libsndfile" ,libsndfile)
       ("libsamplerate" ,libsamplerate)
       ("fftwf" ,fftwf)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://aubio.org/")
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
    (version "5.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.ardour.org/ardour/ardour.git")
                    (commit version)))
              (snippet
               ;; Ardour expects this file to exist at build time.  The revision
               ;; is the output of
               ;;    git describe HEAD | sed 's/^[A-Za-z]*+//'
               `(call-with-output-file
                    "libs/ardour/revision.cc"
                  (lambda (port)
                    (format port ,(string-append "#include \"ardour/revision.h\"
namespace ARDOUR { const char* revision = \"" version "\" ; }")))))
              (sha256
               (base32
                "0mla5lm51ryikc2rrk53max2m7a5ds6i1ai921l2h95wrha45nkr"))
              (file-name (string-append name "-" version))))
    (build-system waf-build-system)
    (arguments
     `(#:configure-flags '("--cxx11"          ; required by gtkmm
                           "--no-phone-home"  ; don't contact ardour.org
                           "--freedesktop"    ; install .desktop file
                           "--test")          ; build unit tests
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'set-rpath-in-LDFLAGS
          ,(ardour-rpath-phase (version-major version))))
       #:test-target "test"
       #:python ,python-2))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("aubio" ,aubio)
       ("lrdf" ,lrdf)
       ("boost" ,boost)
       ("atkmm" ,atkmm)
       ("cairomm" ,cairomm)
       ("eudev" ,eudev)
       ("gtkmm" ,gtkmm-2)
       ("glibmm" ,glibmm)
       ("libart-lgpl" ,libart-lgpl)
       ("libgnomecanvasmm" ,libgnomecanvasmm)
       ("pangomm" ,pangomm)
       ("liblo" ,liblo)
       ("libsndfile" ,libsndfile)
       ("libsamplerate" ,libsamplerate)
       ("libxml2" ,libxml2)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("flac" ,flac)
       ("lv2" ,lv2)
       ("vamp" ,vamp)
       ("curl" ,curl)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("jack" ,jack-1)
       ("serd" ,serd)
       ("sord" ,sord)
       ("sratom" ,sratom)
       ("suil" ,suil)
       ("lilv" ,lilv)
       ("readline" ,readline)
       ("redland" ,redland)
       ("rubberband" ,rubberband)
       ("libarchive" ,libarchive)
       ("taglib" ,taglib)
       ("python-rdflib" ,python-rdflib)))
    (native-inputs
     `(("perl" ,perl)
       ("cppunit" ,cppunit)
       ("itstool" ,itstool)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (home-page "http://ardour.org")
    (synopsis "Digital audio workstation")
    (description
     "Ardour is a multi-channel digital audio workstation, allowing users to
record, edit, mix and master audio and MIDI projects.  It is targeted at audio
engineers, musicians, soundtrack editors and composers.")
    (license license:gpl2+)))

(define-public audacity
  (package
    (name "audacity")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/audacity/audacity/archive"
                           "/Audacity-" version ".tar.gz"))
       (sha256
        (base32 "09xpr4bjnainz1xmc35v3qg3dadjr9wv8bmn1p4y91aqyihnhjry"))
       (patches (search-patches "audacity-build-with-system-portaudio.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled libraries.
        '(begin
           (for-each
            (lambda (dir)
              (delete-file-recursively (string-append "lib-src/" dir)))
            '("expat" "ffmpeg" "libflac" "libid3tag" "libmad" "libogg"
              "libsndfile" "libsoxr" "libvamp" "libvorbis" "lv2"
              "portaudio-v19" "portmidi" "soundtouch" "twolame"
              ;; FIXME: these libraries have not been packaged yet:
              ;; "libnyquist"
              ;; "libscorealign"
              ;; "libwidgetextra"
              ;; "portburn"
              ;; "portsmf"
              ;; "portmixer"

              ;; FIXME: we have this library, but it differs in that the Slide
              ;; class does not have a member "getInverseStretchedTime".
              ;; "sbsms"
              ))
           #t))))
    (build-system gnu-build-system)
    (inputs
     `(("wxwidgets" ,wxwidgets)
       ("gtk" ,gtk+)
       ("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("expat" ,expat)
       ("ffmpeg" ,ffmpeg)
       ("lame" ,lame)
       ("flac" ,flac)
       ("libid3tag" ,libid3tag)
       ("libmad" ,libmad)
       ;;("libsbsms" ,libsbsms)         ;bundled version is modified
       ("libsndfile" ,libsndfile)
       ("soundtouch" ,soundtouch)
       ("soxr" ,soxr)                   ;replaces libsamplerate
       ("twolame" ,twolame)
       ("vamp" ,vamp)
       ("libvorbis" ,libvorbis)
       ("lv2" ,lv2)
       ("lilv" ,lilv)                   ;for lv2
       ("suil" ,suil)                   ;for lv2
       ("portaudio" ,portaudio)
       ("portmidi" ,portmidi)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)     ;for msgfmt
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("which" ,which)))
    (arguments
     `(#:configure-flags
       (let ((libid3tag (assoc-ref %build-inputs "libid3tag"))
             (libmad (assoc-ref %build-inputs "libmad"))
             (portmidi (assoc-ref %build-inputs "portmidi")))
         (list
          ;; Loading FFmpeg dynamically is problematic.
          "--disable-dynamic-loading"
          ;; SSE instructions are available on Intel systems only.
          ,@(if (any (cute string-prefix? <> (or (%current-target-system)
                                                 (%current-system)))
                    '("x64_64" "i686"))
              '()
              '("--enable-sse=no"))
          ;; portmidi, libid3tag and libmad provide no .pc files, so
          ;; pkg-config fails to find them.  Force their inclusion.
          (string-append "ID3TAG_CFLAGS=-I" libid3tag "/include")
          (string-append "ID3TAG_LIBS=-L" libid3tag "/lib -lid3tag -lz")
          (string-append "LIBMAD_CFLAGS=-I" libmad "/include")
          (string-append "LIBMAD_LIBS=-L" libmad "/lib -lmad")
          (string-append "PORTMIDI_CFLAGS=-I" portmidi "/include")
          (string-append "PORTMIDI_LIBS=-L" portmidi "/lib -lportmidi")
          "EXPAT_USE_SYSTEM=yes"
          "FFMPEG_USE_SYSTEM=yes"
          "LAME_USE_SYSTEM=yes"
          "LIBFLAC_USE_SYSTEM=yes"
          "LIBID3TAG_USE_SYSTEM=yes"
          "LIBMAD_USE_SYSTEM=yes"
          "USE_LOCAL_LIBNYQUIST="      ;not packaged yet
          ;;"LIBSBSMS_USE_SYSTEM=yes"  ;bundled version is patched
          "LIBSNDFILE_USE_SYSTEM=yes"
          "LIBSOUNDTOUCH_USE_SYSTEM=yes"
          "LIBSOXR_USE_SYSTEM=yes"
          "LIBTWOLAME_USE_SYSTEM=yes"
          "LIBVAMP_USE_SYSTEM=yes"
          "LIBVORBIS_USE_SYSTEM=yes"
          "LV2_USE_SYSTEM=yes"
          "PORTAUDIO_USE_SYSTEM=yes"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-sbsms-check
           (lambda _
             ;; This check is wrong: there is no 2.2.0 release; not even the
             ;; bundled sources match this release string.
             (substitute* '("m4/audacity_checklib_libsbsms.m4"
                            "configure")
               (("sbsms >= 2.2.0") "sbsms >= 2.0.0"))
             #t))
         (add-after 'unpack 'use-upstream-headers
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("src/NoteTrack.cpp"
                            "src/AudioIO.cpp"
                            "src/AudioIO.h")
               (("../lib-src/portmidi/pm_common/portmidi.h") "portmidi.h")
               (("../lib-src/portmidi/porttime/porttime.h") "porttime.h"))
             (substitute* "src/prefs/MidiIOPrefs.cpp"
               (("../../lib-src/portmidi/pm_common/portmidi.h") "portmidi.h"))
             #t)))
       ;; The test suite is not "well exercised" according to the developers,
       ;; and fails with various errors.  See
       ;; <http://sourceforge.net/p/audacity/mailman/message/33524292/>.
       #:tests? #f))
    (home-page "http://audacity.sourceforge.net/")
    (synopsis "Software for recording and editing sounds")
    (description
     "Audacity is a multi-track audio editor designed for recording, playing
and editing digital audio.  It features digital effects and spectrum analysis
tools.")
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
             "CXXFLAGS=-std=gnu++11"
             "CFLAGS=-std=gnu++11"
             (string-append "prefix=" %output)
             (string-append "pkgdatadir=" %output "/share/azr3-jack"))))
    (inputs
     `(("gtkmm" ,gtkmm-2)
       ("lvtk" ,lvtk)
       ("jack" ,jack-1)
       ("lash" ,lash)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (version "0.90.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://calf-studio-gear.org/files/calf-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0dijv2j7vlp76l10s4v8gbav26ibaqk8s24ci74vrc398xy00cib"))))
    (build-system gnu-build-system)
    (inputs
     `(("fluidsynth" ,fluidsynth)
       ("expat" ,expat)
       ("glib" ,glib)
       ("gtk" ,gtk+-2)
       ("cairo" ,cairo)
       ("lash" ,lash)
       ("jack" ,jack-1)
       ("lv2" ,lv2)
       ("ladspa" ,ladspa)
       ("fftw" ,fftw)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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

(define-public espeak
  (package
    (name "espeak")
    (version "1.48.04")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/espeak/espeak/"
                                  "espeak-" (version-major+minor version)
                                  "/espeak-" version "-source.zip"))
              (sha256
               (base32
                "0n86gwh9pw0jqqpdz7mxggllfr8k0r7pc67ayy7w5z6z79kig6mz"))
              (modules '((guix build utils)))
              (snippet
               ;; remove prebuilt binaries
               '(delete-file-recursively "linux_32bit"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          (string-append "DATADIR="
                                         (assoc-ref %outputs "out")
                                         "/share/espeak-data")
                          (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out")
                                         "/lib")
                          "AUDIO=pulseaudio")
       #:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (chdir "src")
             ;; We use version 19 of the PortAudio library, so we must copy the
             ;; corresponding file to be sure that espeak compiles correctly.
             (copy-file "portaudio19.h" "portaudio.h")
             (substitute* "Makefile"
               (("/bin/ln") "ln"))
             #t)))))
       (inputs
        `(("portaudio" ,portaudio)
          ("pulseaudio" ,pulseaudio)))
       (native-inputs `(("unzip" ,unzip)))
       (home-page "http://espeak.sourceforge.net/")
       (synopsis "Software speech synthesizer")
       (description "eSpeak is a software speech synthesizer for English and
other languages.  eSpeak uses a \"formant synthesis\" method.  This allows many
languages to be provided in a small size.  The speech is clear, and can be used
at high speeds, but is not as natural or smooth as larger synthesizers which are
based on human speech recordings.")
       (license license:gpl3+)))

(define-public infamous-plugins
  (package
    (name "infamous-plugins")
    (version "0.2.02")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ssj71/infamousPlugins/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qm3ak07vc1l3f5c3c2lq9gkfknlxwn8ks03cysw1pk8hj7dwnv6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; There are no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-compiler-flags
           (lambda _
             (substitute* '("src/casynth/CMakeLists.txt"
                            "src/cheapdist/CMakeLists.txt"
                            "src/duffer/CMakeLists.txt"
                            "src/envfollower/CMakeLists.txt"
                            "src/ewham/CMakeLists.txt"
                            "src/hip2b/CMakeLists.txt"
                            "src/lushlife/CMakeLists.txt"
                            "src/powercut/CMakeLists.txt"
                            "src/powerup/CMakeLists.txt"
                            "src/stuck/CMakeLists.txt")
                          (("-msse2 -mfpmath=sse") ""))
             #t)))))
    (inputs
     `(("cairo" ,cairo)
       ("fftwf" ,fftwf)
       ("lv2" ,lv2)
       ("ntk" ,ntk)
       ("zita-resampler" ,zita-resampler)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://ssj71.github.io/infamousPlugins")
    (synopsis "LV2 plugins for live use")
    (description
     "The infamous plugins are a collection of LV2 audio plugins for live
performances.  The plugins include a cellular automaton synthesizer, an
envelope follower, distortion effects, tape effects and more.")
    (license license:gpl2+)))

(define-public swh-plugins-lv2
  (package
    (name "swh-plugins-lv2")
    (version "1.0.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/swh/"
                                  "lv2/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0j1mih0lp4fds07knp5i32in515sh0df1qi6694pmyz2wqnm295w"))))
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
     `(("lv2" ,lv2)
       ("fftwf" ,fftwf)))
    (native-inputs
     `(("libxslt" ,libxslt)
       ("pkg-config" ,pkg-config)))
    (home-page "http://plugin.org.uk")
    (synopsis "SWH plugins in LV2 format")
    (description
     "Swh-plugins-lv2 is a collection of audio plugins in LV2 format.  Plugin
classes include: dynamics (compressor, limiter), time (delay, chorus,
flanger), ringmodulator, distortion, filters, pitchshift, oscillators,
emulation (valve, tape), bit fiddling (decimator, pointer-cast), etc.")
    (license license:gpl3+)))

(define-public csound
  (package
    (name "csound")
    (version "6.09.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/csound/csound/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xqpqws4jsv7fyawcjzwaw544qbfh29xq164kdf30a9v1n3yklp4"))))
    (build-system cmake-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("pulseaudio" ,pulseaudio)
       ("libsndfile" ,libsndfile)
       ("liblo" ,liblo)
       ("ladspa" ,ladspa)
       ("jack" ,jack-1)
       ("gettext" ,gettext-minimal)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("zlib" ,zlib)))
    (home-page "http://csound.github.io/")
    (synopsis "Sound and music computing system")
    (description
     "Csound is a user-programmable and user-extensible sound processing
language and software synthesizer.")
    (license license:lgpl2.1+)))

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
                                     "/lib/libclalsadrv.so.2"))))
         ;; no configure script
         (delete 'configure))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fftw" ,fftw)))
    (home-page "http://kokkinizita.linuxaudio.org")
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
    (home-page "http://kokkinizita.linuxaudio.org")
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
    (home-page "http://kokkinizita.linuxaudio.org")
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
    (home-page "http://kokkinizita.linuxaudio.org")
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
    (home-page "http://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA four-band parametric equalizer plugin")
    (description
     "This package provides a LADSPA plugin for a four-band parametric
equalizer.  Each section has an active/bypass switch, frequency, bandwidth and
gain controls.  There is also a global bypass switch and gain control.

The 2nd order resonant filters are implemented using a Mitra-Regalia style
lattice filter, which is stable even while parameters are being changed.

All switches and controls are internally smoothed, so they can be used 'live'
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
    (home-page "http://kokkinizita.linuxaudio.org")
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
    (home-page "http://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA plugin for synthesizer oscillators")
    (description
     "The @code{blvco} LADSPA plugin provides three anti-aliased oscillators:

@enumerate
@item Pulse-VCO, a dirac pulse oscillator with flat amplitude spectrum
@item Saw-VCO, a sawtooth oscillator with 1/F amplitude spectrum
@item Rec-VCO, a square / rectange oscillator
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
    (home-page "http://kokkinizita.linuxaudio.org")
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
    (home-page "http://kokkinizita.linuxaudio.org")
    (synopsis "LADSPA stereo reverb plugin")
    (description
     "This package provides a LADSPA plugin for a stereo reverb effect.")
    (license license:gpl2+)))

(define-public fluidsynth
  (package
    (name "fluidsynth")
    (version "1.1.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FluidSynth/fluidsynth.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "12q7hv0zvgylsdj1ipssv5zr7ap2y410dxsd63dz22y05fa2hwwd"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f  ; no check phase
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-libdir
           (lambda _
             (substitute* "CMakeLists.txt"
               (("LIB_SUFFIX \\$\\{_init_lib_suffix\\}")
                "LIB_SUFFIX \"\""))
             #t)))))
    (inputs
     `(("libsndfile" ,libsndfile)
       ("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("ladspa" ,ladspa)
       ("lash" ,lash)
       ("readline" ,readline)
       ("glib" ,glib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.fluidsynth.org/")
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
    (version "2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/faac/faad2-src/faad2-"
                                  version "/faad2-" version ".zip"))
              (sha256
               (base32
                "16f3l16c00sg0wkrkm3vzv0gy3g97x309vw788igs0cap2x1ak3z"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("unzip" ,unzip)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _
             (substitute* "bootstrap" (("\r\n") "\n"))
             (zero? (system* "sh" "bootstrap")))))))
    (home-page "http://www.audiocoding.com/faad2.html")
    (synopsis "MPEG-4 and MPEG-2 AAC decoder")
    (description
     "FAAD2 is an MPEG-4 and MPEG-2 AAC decoder supporting LC, Main, LTP, SBR,
PS, and DAB+.")
    (license license:gpl2)))

(define-public faust
  (package
    (name "faust")
    (version "0.9.90")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grame-cncm/faust.git")
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
     `(("unzip" ,unzip)))
    (home-page "http://faust.grame.fr/")
    (synopsis "Signal processing language")
    (description
     "Faust is a programming language for realtime audio signal processing.")
    (license license:gpl2+)))

(define-public faust-2
  (package
    (inherit faust)
    (version "2.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/grame-cncm/faust.git")
                    (commit (string-append "v"
                                           (string-map (lambda (c)
                                                         (if (char=? c #\.) #\- c))
                                                       version)))))
              (sha256
               (base32
                "06km0ygwxxwgw1lqldccqidxhmjfz8ck0wnbd95qk5sg8sbpc068"))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments faust)
       ((#:make-flags flags)
        `(list (string-append "prefix=" (assoc-ref %outputs "out"))
               "world"))))
    (native-inputs
     `(("llvm" ,llvm-with-rtti)
       ("which" ,which)
       ("xxd" ,xxd)
       ("ctags" ,emacs-minimal)  ; for ctags
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libsndfile" ,libsndfile)
       ("libmicrohttpd" ,libmicrohttpd)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))))

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
                     (system* "tar" "xvf" (assoc-ref %build-inputs "source"))
                     (chdir "freepats")
                     ;; Use absolute pattern references
                     (substitute* "freepats.cfg"
                       (("Tone_000") (string-append out "/Tone_000"))
                       (("Drum_000") (string-append out "/Drum_000")))
                     (mkdir-p out)
                     (copy-recursively "." out)))))
    (native-inputs
     `(("tar" ,tar)
       ("bzip2" ,bzip2)))
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
    (version "0.36.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/guitarix/guitarix/guitarix2-"
                   version ".tar.xz"))
             (sha256
              (base32
               "1g5949jwh2n755xjs3kcbdb8a1wxr5mn0m115wdnk27dxcdn93b0"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:python ,python-2
       #:configure-flags
       (list
        ;; Add the output lib directory to the RUNPATH.
        (string-append "--ldflags=-Wl,-rpath=" %output "/lib")
        "--cxxflags=-std=c++11")))
    (inputs
     `(("libsndfile" ,libsndfile)
       ("boost" ,boost)
       ("avahi" ,avahi)
       ("eigen" ,eigen)
       ("lv2" ,lv2)
       ("lilv" ,lilv)
       ("ladspa" ,ladspa)
       ("jack" ,jack-1)
       ("gtkmm" ,gtkmm-2)
       ("gtk+" ,gtk+-2)
       ("fftwf" ,fftwf)
       ("lrdf" ,lrdf)
       ("zita-resampler" ,zita-resampler)
       ("zita-convolver" ,zita-convolver)))
    (native-inputs
     `(("gperf" ,gperf)
       ("faust" ,faust)
       ("intltool" ,intltool)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))
    (home-page "http://guitarix.org/")
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
        `(cons "--lv2-only" ,flags))))))

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
     `(("alsa-utils" ,alsa-utils)
       ("fltk" ,fltk)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxft" ,libxft)
       ("libxrender" ,libxrender)
       ("libxpm" ,libxpm)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("jack" ,jack-1)
       ("alsa-lib" ,alsa-lib)
       ("libsndfile" ,libsndfile)
       ("libsamplerate" ,libsamplerate)
       ("zlib" ,zlib)))
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
    (version "1.3.2")
    (source (origin
             (method url-fetch)
             ;; The original home-page is gone. Download the tarball from an
             ;; archive mirror instead.
             (uri (list (string-append
                         "https://web.archive.org/web/20150803095032/"
                         "http://factorial.hu/system/files/ir.lv2-"
                         version ".tar.gz")
                        (string-append
                         "https://mirrors.kernel.org/gentoo/distfiles/ir.lv2-"
                         version ".tar.gz")))
             (sha256
              (base32
               "1jh2z01l9m4ar7yz0n911df07dygc7n4cl59p7qdjbh0nvkm747g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                              ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))        ; no configure script
    (inputs
     `(("libsndfile" ,libsndfile)
       ("libsamplerate" ,libsamplerate)
       ("lv2" ,lv2)
       ("glib" ,glib)
       ("gtk+" ,gtk+-2)
       ("zita-convolver" ,zita-convolver)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))
    ;; Link to an archived copy of the home-page since the original is gone.
    (home-page (string-append "https://web.archive.org/web/20150803095032/"
                              "http://factorial.hu/plugins/lv2/ir"))
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
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://jackaudio.org/downloads/jack-audio-connection-kit-"
                   version
                   ".tar.gz"))
             (sha256
              (base32
               "0i6l25dmfk2ji2lrakqq9icnwjxklgcjzzk65dmsff91z2zva5rm"))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("readline" ,readline)))
    ;; uuid.h is included in the JACK type headers
    ;; db.h is included in the libjack metadata headers
    (propagated-inputs
     `(("libuuid" ,util-linux)
       ("bdb" ,bdb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://jackaudio.org/")
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
    (version "1.9.12")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/jackaudio/jack2/releases/"
                                 "download/v" version "/jack2-"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "0crf4y9a5j9miw8r5ji4l3w5w0y2frrf7xyfsfdgacnw6vwy5vyy"))))
    (build-system waf-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f  ; no check target
       #:configure-flags '("--dbus"
                           "--alsa")
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-linkflags
          (lambda _
            ;; Add $libdir to the RUNPATH of all the binaries.
            (substitute* "wscript"
              ((".*CFLAGS.*-Wall.*" m)
               (string-append m
                              "    conf.env.append_unique('LINKFLAGS',"
                              "'-Wl,-rpath=" %output "/lib')\n")))))
         (add-after 'install 'wrap-python-scripts
          (lambda* (#:key inputs outputs #:allow-other-keys)
            ;; Make sure 'jack_control' runs with the correct PYTHONPATH.
            (let* ((out (assoc-ref outputs "out"))
                   (path (getenv "PYTHONPATH")))
              (wrap-program (string-append out "/bin/jack_control")
                `("PYTHONPATH" ":" prefix (,path))))
            #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("expat" ,expat)
       ("libsamplerate" ,libsamplerate)
       ("opus" ,opus)
       ("python2-dbus" ,python2-dbus)
       ("readline" ,readline)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    ;; Most files are under GPLv2+, but some headers are under LGPLv2.1+
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public jalv
  (package
    (name "jalv")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.drobilla.net/jalv-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1x2wpzzx2cgvz3dgdcgsj8dr0w3zsasy62mvl199bsdj5fbjaili"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-flags
          (lambda _
            ;; Compile with C++11, required by gtkmm.
            (setenv "CXXFLAGS" "-std=c++11")
            #t)))))
    (inputs
     `(("lv2" ,lv2)
       ("lilv" ,lilv)
       ("suil" ,suil)
       ("gtk" ,gtk+)
       ("gtkmm" ,gtkmm)
       ("jack" ,jack-1)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://drobilla.net/software/jalv/")
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
       (alist-replace
        'configure
        (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
          (chdir "src")
          (let ((out (assoc-ref outputs "out")))
            (substitute* "makefile"
              (("/usr/lib/ladspa/") (string-append out "/lib/ladspa/"))
              (("/usr/include/")    (string-append out "/include/"))
              (("/usr/bin/")        (string-append out "/bin/"))
              (("-mkdirhier")       "mkdir -p")
              (("^CC.*")            "CC = gcc\n")
              (("^CPP.*")           "CPP = g++\n"))))
        (alist-delete 'build %standard-phases))))
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
     '(#:phases
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
       ("readline" ,readline)
       ("python" ,python-2)))
    ;; According to pkg-config, packages depending on lash also need to have
    ;; at least the following packages declared as inputs.
    (propagated-inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.nongnu.org/lash/")
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
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libsndfile" ,libsndfile)))
    (home-page "https://sourceforge.net/projects/bs2b/")
    (synopsis "Bauer stereophonic-to-binaural DSP")
    (description
     "The Bauer stereophonic-to-binaural DSP (bs2b) library and plugins is
designed to improve headphone listening of stereo audio records.  Recommended
for headphone prolonged listening to disable superstereo fatigue without
essential distortions.")
    (license license:expat)))

(define-public liblo
  (package
    (name "liblo")
    (version "0.28")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/liblo/liblo/" version
                                 "/liblo-" version ".tar.gz"))
             (sha256
              (base32
               "02drgnpirvl2ihvzgsmn02agr5sj3vipzzw9vma56qlkgfvak56s"))))
    (build-system gnu-build-system)
    (arguments
     `(;; liblo test FAILED
       ;; liblo server error 19 in setsockopt(IP_ADD_MEMBERSHIP): No such device
       #:tests? #f))
    (home-page "http://liblo.sourceforge.net")
    (synopsis "Implementation of the Open Sound Control protocol")
    (description
     "liblo is a lightweight library that provides an easy to use
implementation of the Open Sound Control (OSC) protocol.")
    (license license:lgpl2.1+)))

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
     `(("python-cython" ,python-cython)))
    (inputs
     `(("liblo" ,liblo)))
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

(define-public lilv
  (package
    (name "lilv")
    (version "0.24.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.drobilla.net/lilv-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "08m5a372pr1l7aii9s3pic5nm68gynx1n1bc7bnlswziq6qnbv7p"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-ldflags
          (lambda* (#:key outputs #:allow-other-keys)
            (setenv "LDFLAGS"
                    (string-append "-Wl,-rpath="
                                   (assoc-ref outputs "out") "/lib")))))))
    ;; required by lilv-0.pc
    (propagated-inputs
     `(("serd" ,serd)
       ("sord" ,sord)
       ("sratom" ,sratom)))
    (inputs
     `(("lv2" ,lv2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://drobilla.net/software/lilv/")
    (synopsis "Library to simplify use of LV2 plugins in applications")
    (description
     "Lilv is a C library to make the use of LV2 plugins as simple as possible
for applications.  Lilv is the successor to SLV2, rewritten to be
significantly faster and have minimal dependencies.")
    (license license:isc)))

(define-public lv2
  (package
    (name "lv2")
    (version "1.14.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://lv2plug.in/spec/lv2-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0chxwys3vnn3nxc9x2vchm74s9sx0vfra6y893byy12ci61jc1dq"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f  ; no check target
       #:configure-flags '("--no-plugins")))
    (inputs
     ;; Leaving off cairo and gtk+-2.0 which are needed for example plugins
     `(("libsndfile" ,libsndfile)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://lv2plug.in/")
    (synopsis "LV2 audio plugin specification")
    (description
     "LV2 is an open specification for audio plugins and host applications.
At its core, LV2 is a simple stable interface, accompanied by extensions which
add functionality to support the needs of increasingly powerful audio
software.")
    (license license:isc)))

(define-public lv2-devel
  (let ((commit "39c7c726cd52b2863fcea356cafe1bcab2ba7f37")
        (revision "1"))
    (package (inherit lv2)
      (name "lv2-devel")
      (version (string-append "1.15.3-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://lv2plug.in/git/lv2.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1gp2rd99dfmpibvpixrqn115mrhybzf3if3h8bssf6siyi13f29r")))))))

(define-public lv2-mda-piano
  (package
    (name "lv2-mda-piano")
    (version "0.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "http://git.elephly.net/software/lv2-mdametapiano.git")
                    (commit version)))
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
     `(("lv2" ,lv2)
       ("lvtk" ,lvtk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (native-search-paths
     (list (search-path-specification
            (variable "LV2_PATH")
            (files '("lib/lv2")))))
    (home-page "http://elephly.net/lv2/mdapiano.html")
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
    (home-page "http://elephly.net/lv2/mdaepiano.html")
    (synopsis "LV2 port of the mda EPiano plugin")
    (description "An LV2 port of the mda EPiano VSTi.")))

(define-public lvtk
  (package
    (name "lvtk")
    (version "1.2.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/lvtk/lvtk/archive/"
                                 version
                                 ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "03nbj2cqcklqwh50zj2gwm07crh5iwqbpxbpzwbg5hvgl4k4rnjd"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f  ; no check target
       #:python ,python-2
       #:configure-flags
       (list (string-append "--boost-includes="
                            (assoc-ref %build-inputs "boost")
                            "/include"))
       #:phases (modify-phases %standard-phases
                  (add-before
                   'configure 'set-flags
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; See e.g. https://github.com/lvtk/lvtk/issues/21
                     (setenv "LDFLAGS"
                             (string-append
                              "-L" (assoc-ref inputs "boost") "/lib "
                              "-lboost_system"))
                     ;; Needed for gtkmm
                     (substitute* '("src/wscript_build"
                                    "examples/wscript_build")
                       (("cxxflags.*= \\[" line)
                        (string-append line "\"-std=c++11\", ")))
                     #t)))))
    (inputs
     `(("boost" ,boost)
       ("gtkmm" ,gtkmm-2)
       ("lv2" ,lv2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (version "1.17.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kcat.strangesoft.net/openal-releases/openal-soft-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "051k5fy8pk4fd9ha3qaqcv08xwbks09xl5qs4ijqq2qz5xaghhd3"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f  ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'use-full-library-paths
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "Alc/backends/pulseaudio.c"
              (("#define PALIB \"libpulse\\.so\\.0\"")
               (string-append "#define PALIB \""
                              (assoc-ref inputs "pulseaudio")
                              "/lib/libpulse.so.0"
                              "\"")))
            (substitute* "Alc/backends/alsa.c"
              (("LoadLib\\(\"libasound\\.so\\.2\"\\)")
               (string-append "LoadLib(\""
                              (assoc-ref inputs "alsa-lib")
                              "/lib/libasound.so.2"
                              "\")")))
            #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("pulseaudio" ,pulseaudio)))
    (synopsis "3D audio API")
    (description
     "OpenAL provides capabilities for playing audio in a virtual 3D
environment.  Distance attenuation, doppler shift, and directional sound
emitters are among the features handled by the API.  More advanced effects,
including air absorption, occlusion, and environmental reverb, are available
through the EFX extension.  It also facilitates streaming audio, multi-channel
buffers, and audio capture.")
    (home-page "http://kcat.strangesoft.net/openal.html")
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
     `(("openal" ,openal)))
    (synopsis "Free implementation of OpenAL's ALUT standard")
    (description "freealut is the OpenAL Utility Toolkit.")
    (home-page "http://kcat.strangesoft.net/openal.html")
    (license license:lgpl2.0)))

(define-public patchage
  (package
    (name "patchage")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.drobilla.net/patchage-"
                                  version
                                  ".tar.bz2"))
              (sha256
               (base32
                "1agdpwwi42176l4mxj0c4fsvdiv1ig56bfnnx0msckxmy57df8bb"))))
    (build-system waf-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("jack" ,jack-1)
       ("ganv" ,ganv)
       ("glibmm" ,glibmm)
       ("gtkmm" ,gtkmm-2)
       ("dbus-glib" ,dbus-glib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://drobilla.net/software/patchage/")
    (synopsis "Modular patch bay for audio and MIDI systems")
    (description
     "Patchage is a modular patch bay for audio and MIDI systems based on JACK
and ALSA.")
    (license license:gpl3+)))

(define-public qjackctl
  (package
    (name "qjackctl")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/qjackctl/qjackctl/"
                                  version "/qjackctl-" version ".tar.gz"))
              (sha256
               (base32
                "1dsavjfzz5bpzc80mvfs940w9f9f47cf4r9cqxnaqrl4xilsa3f5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f)) ; no check target
    (inputs
     `(("jack" ,jack-1)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (home-page "https://qjackctl.sourceforge.io/")
    (synopsis "Jack server control application")
    (description "Control a Jack server.  Allows you to plug various sources
into various outputs and to start, stop and configure jackd")
    (license license:gpl2+)))

(define-public qjackrcd
  (package
    (name "qjackrcd")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/qjackrcd/stable/"
                                  "qjackrcd-" version ".tar.gz"))
              (sha256
               (base32
                "0xpnhzbwg5c60n5dhwln5p7qm191nvmf23la88zxfqx1jv0mmxxb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "qmake"
                             (string-append "PREFIX="
                                            (assoc-ref outputs "out")))))))))
    (native-inputs
     `(("qtbase" ,qtbase))) ; for qmake
    (inputs
     `(("jack" ,jack-1)
       ("libsndfile" ,libsndfile)
       ("qtbase" ,qtbase)))
    (home-page "https://sourceforge.net/projects/qjackrcd/")
    (synopsis "Stereo audio recorder for JACK")
    (description "QJackRcd is a simple graphical stereo recorder for JACK
supporting silence processing for automatic pause, file splitting, and
background file post-processing.")
    (license license:gpl2+)))

(define-public raul
  (package
    (name "raul")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.drobilla.net/raul-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "09ms40xc1x6qli6lxkwn5ibqh62nl9w7dq0b6jh1q2zvnrxwsd8b"))))
    (build-system waf-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f)) ; no check target
    (inputs
     `(("glib" ,glib)
       ("boost" ,boost)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://drobilla.net/software/raul/")
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
                      (url "http://git.drobilla.net/raul.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "04fajrass3ymr72flx5js5vxc601ccrmx8ny8scp0rw7j0igyjdr")))))))

(define-public rubberband
  (package
    (name "rubberband")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://bitbucket.org/breakfastquay/rubberband/get/v"
                              version
                              ".tar.bz2"))
              (sha256
               (base32
                "05amrbrxx0da3w7m237q51799r8xgs4ffqabi2qv06hq8dpcj386"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (inputs
     `(("ladspa" ,ladspa)
       ("libsamplerate" ,libsamplerate)
       ("vamp" ,vamp)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://breakfastquay.com/rubberband/")
    (synopsis "Audio time-stretching and pitch-shifting library")
    (description
     "Rubber Band is a library and utility program that permits changing the
tempo and pitch of an audio recording independently of one another.")
    (license license:gpl2+)))

(define-public rtmidi
  (package
    (name "rtmidi")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://github.com/powertab/rtmidi/archive/"
                              version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d49lapnmdgmjxh4vw57h6xk74nn5r0zwysv7jbd7m8kqhpq5rjj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'autoconf
                   (lambda _ (zero? (system* "autoreconf" "-vfi"))))
                  (add-before
                   'build 'fix-makefile
                   (lambda _
                     (substitute* "Makefile"
                       (("/bin/ln") "ln")
                       (("RtMidi.h RtError.h") "RtMidi.h"))
                     #t))
                  (add-before
                   'install 'make-target-dirs
                   (lambda _
                     (let ((out (assoc-ref %outputs "out")))
                       (mkdir-p (string-append out "/bin"))
                       (mkdir (string-append out "/lib"))
                       (mkdir (string-append out "/include")))
                     #t)))))
    (inputs
     `(("jack" ,jack-1)
       ("alsa-lib" ,alsa-lib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/powertab/rtmidi")
    (synopsis "Cross-platform MIDI library for C++")
    (description
     "RtMidi is a set of C++ classes (RtMidiIn, RtMidiOut, and API specific
classes) that provide a common cross-platform API for realtime MIDI
input/output.")
    (license license:expat)))

(define-public sratom
  (package
    (name "sratom")
    (version "0.6.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.drobilla.net/sratom-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0hrxd9i66s06bpn6i3s9ka95134g3sm8yscmif7qgdzhyjqw42j4"))))
    (build-system waf-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (inputs
     `(("lv2" ,lv2)
       ("serd" ,serd)
       ("sord" ,sord)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://drobilla.net/software/sratom/")
    (synopsis "Library for serialising LV2 atoms to/from RDF")
    (description
     "Sratom is a library for serialising LV2 atoms to/from RDF, particularly
the Turtle syntax.")
    (license license:isc)))

(define-public suil
  (package
    (name "suil")
    (version "0.10.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.drobilla.net/suil-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0j489gm3fhnmwmbgw30bvd4byw1vsy4yazdlnji8jzhcz0qwb5cq"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:configure-flags
       '("CXXFLAGS=-std=gnu++11")))
    (inputs
     `(("lv2" ,lv2)
       ("gtk+" ,gtk+-2)
       ("gtk+" ,gtk+)
       ("qt" ,qtbase)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://drobilla.net/software/suil/")
    (synopsis "Library for loading and wrapping LV2 plugin UIs")
    (description
     "Suil is a lightweight C library for loading and wrapping LV2 plugin UIs.

Suil makes it possible to load a UI of a toolkit in a host using another
toolkit.  The API is designed such that hosts do not need to explicitly
support specific toolkits – if Suil supports a particular toolkit, then UIs in
that toolkit will work in all hosts that use Suil automatically.

Suil currently supports every combination of Gtk, Qt, and X11.")
    (license license:isc)))

(define-public timidity++
  (package
    (name "timidity++")
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/timidity/TiMidity++"
                                  "/TiMidity++-" version
                                  "/TiMidity++-" version ".tar.bz2"))
              (sha256
               (base32
                "0xk41w4qbk23z1fvqdyfblbz10mmxsllw0svxzjw5sa9y11vczzr"))))
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
     `(("alsa-lib" ,alsa-lib)
       ("ao" ,ao)
       ("flac" ,flac)
       ("jack" ,jack-1)
       ("libogg" ,libogg)
       ("speex" ,speex)
       ("ncurses" ,ncurses)
       ("freepats" ,freepats)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
     `(("libsndfile" ,libsndfile)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://vamp-plugins.org")
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
    (native-inputs `(("automake" ,automake)))
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
             (string-append (assoc-ref inputs "automake") "/share/automake-"
                            ,(package-version automake) "/ar-lib")
             "ar-lib"))))))
    (home-page "http://sbsms.sourceforge.net/")
    (synopsis "Library for time stretching and pitch scaling of audio")
    (description
     "SBSMS (Subband Sinusoidal Modeling Synthesis) is software for time
stretching and pitch scaling of audio.  This package contains the library.")
    ;; There is no explicit declaration of a license, but a COPYING file
    ;; containing gpl2.
    (license license:gpl2)))

(define-public wavpack
  (package
    (name "wavpack")
    (version "5.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.wavpack.com/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0i19c6krc0p9krwrqy9s5xahaafigqzxcn31piidmlaqadyn4f8r"))))
    (build-system gnu-build-system)
    (home-page "http://www.wavpack.com/")
    (synopsis "Hybrid lossless audio codec")
    (description
     "WavPack is an audio compression format with lossless, lossy and hybrid
compression modes.  This package contains command-line programs and library to
encode and decode wavpack files.")
    (license license:bsd-3)))

(define-public libmodplug
  (package
    (name "libmodplug")
    (version "0.8.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/modplug-xmms/"
                    name "/" version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bfsladg7h6vnii47dd66f5vh1ir7qv12mfb8n36qiwrxq92sikp"))))
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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libxmp" ,libxmp)
       ("pulseaudio" ,pulseaudio)))
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "http://www.surina.net/soundtouch/soundtouch-" version ".tar.gz"))
       (sha256
        (base32 "0sqn3wk4qz20vf0vz853l6dl1gnj1yhqxfwxqsc5lp529kbn2h9x"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("file" ,file)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _
             (zero? (system* "sh" "bootstrap")))))))
    (home-page "http://www.surina.net/soundtouch/")
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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ao" ,ao)
       ("flac" ,flac)
       ("lame" ,lame)
       ("libid3tag" ,libid3tag)
       ("libltdl" ,libltdl)
       ("libmad" ,libmad)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("pulseaudio" ,pulseaudio)))
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
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/soxr/soxr-" version
                       "-Source.tar.xz"))
       (sha256
        (base32 "0xf2w3piwz9gfr1xqyrj4k685q5dy53kq3igv663i4f4y4sg9rjl"))))
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
    (version "0.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/twolame/twolame/" version
                           "/twolame-" version ".tar.gz"))
       (sha256
        (base32 "0ahiqqng5pidwhj1wzph4vxxgxxgcfa3gl0gywipzx2ii7s35wwq"))
       (modules '((guix build utils)))
       ;; The tests break with Perl 5.24:
       ;; https://github.com/njh/twolame/issues/21
       ;; TODO: Remove this snippet when upgrading twolame.
       (snippet
        '(begin
           (substitute* "tests/test.pl" (("\\(@_\\)") "($_[0])"))
           #t))))
    (build-system gnu-build-system)
    (inputs
     `(("libsndfile" ,libsndfile)))
    (native-inputs
     `(("perl" ,perl)
       ("which" ,which)))               ;used in tests/test.pl
    (home-page "http://www.twolame.org/")
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
     `(("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (arguments
     '(#:phases
       ;; Autoreconf is necessary because the audacity-compat patch modifies
       ;; .in files.
       (modify-phases %standard-phases
         (add-after 'unpack 'autoreconf
           (lambda _
             (zero? (system* "autoreconf" "-vif")))))
       #:tests? #f))                    ;no 'check' target
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
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/qsynth/qsynth/" version
                           "/qsynth-" version ".tar.gz"))
       (sha256
        (base32
         "0qhfnikx3xcllkvs60kj6vcf2rwwzh31y41qkk6kwfhzgd219y8f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" phase
       #:configure-flags
       '("CXXFLAGS=-std=gnu++11")))
    (native-inputs
     `(("qttools" ,qttools)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("fluidsynth" ,fluidsynth)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (home-page "http://qsynth.sourceforge.net")
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
       (method url-fetch)
       (uri (string-append "https://github.com/Themaister/RSound/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version))
       (sha256
        (base32 "1wzs40c0k5zpkmm5ffl6c17xmr399sxli7ys0fbb9ib0fd334knx"))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("ao" ,ao)
       ("libsamplerate" ,libsamplerate)
       ("openal" ,openal)
       ("portaudio" ,portaudio)
       ("pulseaudio" ,pulseaudio)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (zero?
              (system* "./configure"
                       (string-append "--prefix=" (assoc-ref outputs "out")))))))
       ;; No 'check' target.
       #:tests? #f))
    (home-page "http://themaister.net/rsound.html")
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
              (method url-fetch)
              (uri (string-append
                    "https://github.com/johnhldavis/xjackfreak/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xj6gpxfnw9jbdgwgm0x23xgfvj2kwmwb1nk0drw8lxgcchkq7d9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "docdir=" (assoc-ref %outputs "out")
                            "/share/doc/xjackfreak"))))
    (inputs
     `(("jack" ,jack-1)
       ("libx11" ,libx11)
       ("libxt" ,libxt)
       ("libxext" ,libxext)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/zita-convolver-"
                    version ".tar.bz2"))
              (snippet
               ;; Don't optimize for a specific processor architecture.
               '(substitute* "libs/Makefile"
                  (("^CXXFLAGS \\+= -march=native") "")))
              (modules '((guix build utils)))
              (sha256
               (base32
                "14qrnczhp5mbwhky64il7kxc4hl1mmh495v60va7i2qnhasr6zmz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile-and-enter-directory
           (lambda _
             (substitute* "libs/Makefile"
               (("ldconfig") "true")
               (("^LIBDIR =.*") "LIBDIR = lib\n"))
             (chdir "libs") #t))
         (add-after 'install 'install-symlink
           (lambda _
             (symlink "libzita-convolver.so"
                      (string-append (assoc-ref %outputs "out")
                                     "/lib/libzita-convolver.so.3"))))
         ;; no configure script
         (delete 'configure))))
    (inputs `(("fftwf" ,fftwf)))
    (home-page "http://kokkinizita.linuxaudio.org")
    (synopsis "Fast, partitioned convolution engine library")
    (description
     "Zita convolver is a C++ library providing a real-time convolution
engine.")
    (license license:gpl3+)))

(define-public zita-resampler
  (package
    (name "zita-resampler")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/zita-resampler-"
                    version ".tar.bz2"))
              (snippet
               ;; Don't optimize for a specific processor architecture.
               '(substitute* '("apps/Makefile" "libs/Makefile")
                  (("^CXXFLAGS \\+= -march=native") "")))
              (modules '((guix build utils)))
              (sha256
               (base32
                "0r9ary5sc3y8vba5pad581ha7mgsrlyai83w7w4x2fmhfy64q0wq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-makefile-and-enter-directory
          (lambda _
            (substitute* "libs/Makefile"
              (("ldconfig") "true")
              (("^LIBDIR =.*") "LIBDIR = lib\n"))
            (chdir "libs")
            #t))
         (add-after
          'install 'install-symlink
          (lambda _
            (symlink "libzita-resampler.so"
                     (string-append (assoc-ref %outputs "out")
                                    "/lib/libzita-resampler.so.1"))))
         ;; no configure script
         (delete 'configure))))
    (home-page "http://kokkinizita.linuxaudio.org/linuxaudio/zita-resampler/resampler.html")
    (synopsis "C++ library for resampling audio signals")
    (description
     "Libzita-resampler is a C++ library for resampling audio signals.  It is
designed to be used within a real-time processing context, to be fast, and to
provide high-quality sample rate conversion.")
    (license license:gpl3+)))

(define-public zita-alsa-pcmi
  (package
    (name "zita-alsa-pcmi")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kokkinizita.linuxaudio.org"
                    "/linuxaudio/downloads/zita-alsa-pcmi-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1rgv332g82rrrlm4vdam6p2pyrisxbi7b3izfaa0pcjglafsy7j9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefile-and-enter-directory
           (lambda _
             (substitute* "libs/Makefile"
               (("ldconfig") "true")
               (("^LIBDIR =.*") "LIBDIR = lib\n"))
             (chdir "libs")
             #t))
         (add-after 'install 'install-symlink
           (lambda _
             (symlink "libzita-alsa-pcmi.so"
                      (string-append (assoc-ref %outputs "out")
                                     "/lib/libzita-alsa-pcmi.so.0"))))
          ;; no configure script
          (delete 'configure))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fftw" ,fftw)))
    (home-page "http://kokkinizita.linuxaudio.org")
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
             (method url-fetch)
             (uri (string-append "https://github.com/svend/cuetools/archive/"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "01xi3rvdmil9nawsha04iagjylqr1l9v9vlzk99scs8c207l58i4"))))
    (build-system gnu-build-system)
    ;; The source tarball is not bootstrapped.
    (arguments
     `(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'bootstrap
            (lambda _ (zero? (system* "autoreconf" "-vfi")))))))
    ;; Bootstrapping tools
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("flex" ,flex)
       ("bison" ,bison)))
    (synopsis "Cue and toc file parsers and utilities")
    (description "Cuetools is a set of programs that are useful for manipulating
and using CUE sheet (cue) files and Table of Contents (toc) files.  CUE and TOC
files are a way to represent the layout of a data or audio CD in a
machine-readable ASCII format.")
    (home-page "https://github.com/svend/cuetools")
    (license license:gpl2+)))

(define-public shntool
  (package
    (name "shntool")
    (version "3.0.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://etree.org/shnutils/shntool/dist/src/"
                                 "shntool-" version ".tar.gz"))
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
              (method url-fetch)
              (uri (string-append "https://github.com/foo86/dcadec/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i0dpypgqkhhi4v1fmsp2way6w9kbcix3c7q79pmg39yvrzj17gd"))))
    (build-system gnu-build-system)
    (arguments
     ;; Test files are missing: https://github.com/foo86/dcadec/issues/53
     '(#:tests? #f
       #:make-flags
       (list "CC=gcc"
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

(define-public bs1770gain
  (package
    (name "bs1770gain")
    (version "0.4.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/bs1770gain/bs1770gain/"
                           version "/bs1770gain-" version ".tar.gz"))
       (sha256
        (base32
         "0n9skdap1vnl6w52fx0gsrjlk7w3xgdwi62ycyf96h29rx059z6a"))))
    (build-system gnu-build-system)
    (inputs `(("ffmpeg" ,ffmpeg)
              ("sox" ,sox)))
    (home-page "http://bs1770gain.sourceforge.net/")
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
               (url "https://github.com/irungentoo/filter_audio.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0hbb290n3wb23f2k692a6bhc23nnqmxqi9sc9j15pnya8wifw64g"))))
      (build-system gnu-build-system)
      (arguments
       '(#:make-flags (list (string-append "PREFIX=" %output)
                            "CC=gcc")
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
    (version "1.0.14")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "http://www.quut.com/" name "/" name
                       "-" version ".tar.gz"))
       (sha256
        (base32
         "0b1mx69jq88wva3wk0hi6fcl5a52qhnq2f9p3f3jdh5k61ma252q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "tst"
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'pre-install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (substitute* "Makefile"
                 (("INSTALL_ROOT\t=")
                  (string-append "INSTALL_ROOT\t=\t" out)))
               (mkdir-p (string-append out "/inc"))
               (mkdir-p (string-append out "/man"))
               (mkdir-p (string-append out "/man/man1"))
               (mkdir-p (string-append out "/man/man3"))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/lib")))))
         (add-after 'install 'post-install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (rename-file (string-append out "/inc")
                            (string-append out "/include"))
               (mkdir-p (string-append out "/include/gsm"))
               (copy-recursively "inc"
                                 (string-append out "/include/gsm")))))
         (delete 'configure))))
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
     `(("alsa-lib" ,alsa-lib)))
    (home-page "http://larsimmisch.github.io/pyalsaaudio/")
    (synopsis "ALSA wrappers for Python")
    (description
     "This package contains wrappers for accessing the ALSA API from Python.
It is currently fairly complete for PCM devices, and has some support for
mixers.")
    (license license:psfl)))

(define-public python2-pyalsaaudio
  (package-with-python2 python-pyalsaaudio))

(define-public bluez-alsa
  (package
    (name "bluez-alsa")
    (version "1.2.0")
    (source (origin
              ;; The tarballs are mere snapshots and don't contain a
              ;; bootstrapped build system.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Arkq/bluez-alsa.git")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1qinf41wl2ihx54zmmhanycihwjkn7dn1cicq6pp4rqbiv79b95x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootstrap
           (lambda _
             (zero? (system* "autoreconf" "-vif")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bluez" ,bluez)
       ("glib" ,glib)
       ("libbsd" ,libbsd)
       ("ncurses" ,ncurses)
       ("ortp" ,ortp)
       ("sbc" ,sbc)))
    (home-page "https://github.com/Arkq/bluez-alsa")
    (synopsis "Bluetooth ALSA backend")
    (description "This project is a rebirth of a direct integration between
Bluez and ALSA.  Since Bluez >= 5, the build-in integration has been removed
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
    (version "17.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ccrma-ftp.stanford.edu/pub/Lisp/"
                                  "snd-" version ".tar.gz"))
              (sha256
               (base32
                "1vm0dy5qlycqkima7y5ajzvazyjybifa803fabjcpncjz08c26vp"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:out-of-source? #f              ; for the 'install-doc' phase
       #:configure-flags
       (let* ((out (assoc-ref %outputs "out"))
              (docdir (string-append out "/share/doc/snd")))
         (list "--with-alsa" "--with-jack" "--with-gmp"
               (string-append "--with-doc-dir=" docdir)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (docdir (string-append out "/share/doc/snd")))
               (mkdir-p docdir)
               (for-each
                (lambda (f)
                  (install-file f docdir))
                (find-files "." "\\.html$|COPYING"))
               (copy-recursively "pix" (string-append docdir "/pix"))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fftw" ,fftw)
       ("flac" ,flac)
       ("gmp" ,gmp)
       ("gsl" ,gsl)
       ("gtk+" ,gtk+)
       ("jack" ,jack-1)
       ("libsamplerate" ,libsamplerate)
       ("mpc" ,mpc)
       ("mpfr" ,mpfr)
       ("mpg123" ,mpg123)
       ("speex" ,speex)
       ("timidity++" ,timidity++)
       ("vorbis-tools" ,vorbis-tools)
       ("wavpack" ,wavpack)))
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
    (version "0.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/lucianodato/noise-repellent.git")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0rd3dlmk3vivjmcr6x2x860y0j1d49c2j95j6ny50v184mwvn11j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("lv2" ,lv2)
       ("fftwf" ,fftwf)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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

(define-public cli-visualizer
  (package
    (name "cli-visualizer")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dpayne/cli-visualizer/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07zkm87f2fr8kc6531zrkya7q81sdanm6813y2f54mg13g41y6hi"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)))
    (inputs
     `(("fftw" ,fftw)
       ("googletest" ,googletest)
       ("ncurses" ,ncurses)
       ("pulseaudio" ,pulseaudio)))
    (arguments
     '(#:test-target "test"
       #:make-flags
       (list (string-append "PREFIX=" %output "/bin/") "ENABLE_PULSE=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-sudo
           (lambda _
             (substitute* "install.sh" (("sudo") ""))
             #t))
         (add-before 'check 'set-check-environment
           (lambda _
             (setenv "CXX" "g++")
             (setenv "CC" "gcc")
             #t))
         (add-before 'install 'make-prefix
           (lambda _
             (mkdir-p (string-append (assoc-ref %outputs "out") "/bin"))
             #t))
         (add-after 'install 'data
           (lambda _
             (for-each (lambda (file)
                         (install-file file
                                       (string-append (assoc-ref %outputs "out")
                                                      "/share/doc")))
                       (find-files "examples"))
             #t)))))
    (home-page "https://github.com/dpayne/cli-visualizer/")
    (synopsis "Command-line audio visualizer")
    (description "@code{cli-visualizer} displays fast-Fourier
transforms (FFTs) of the sound being played, as well as other graphical
representations.")
    (license license:expat)))
