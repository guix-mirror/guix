;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3) ;taglib
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)  ;libsndfile, libsamplerate
  #:use-module (gnu packages python)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages zip)
  #:use-module (srfi srfi-1))

(define-public alsa-modular-synth
  (package
    (name "alsa-modular-synth")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/alsamodular/ams-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1nb7qzzqlqa2x8h797jbwi18ihnfkxqg9lyi0c4nvf8ybwzxkzd2"))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ;; We cannot use zita-alsa-pcmi (the successor of clalsadrv) due to
       ;; license incompatibility.
       ("clalsadrv" ,clalsadrv)
       ("fftw" ,fftw)
       ("jack" ,jack-1)
       ("ladspa" ,ladspa)
       ("liblo" ,liblo)
       ("qt" ,qt-4)))
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
    (synopsis "A library for audio labelling")
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

(define-public ardour-3
  (package
    (name "ardour")
    (version "3.5.403")
    (source (origin
              ;; The project only provides tarballs upon individual request
              ;; (or after payment) so we take the code from git.
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.ardour.org/ardour/ardour.git")
                    (commit version)))
              (snippet
               ;; Ardour expects this file to exist at build time.  It can be
               ;; created from a git checkout with:
               ;;   ./waf create_stored_revision
               '(call-with-output-file
                    "libs/ardour/revision.cc"
                  (lambda (port)
                    (format port "#include \"ardour/revision.h\"
namespace ARDOUR { const char* revision = \"3.5-403-gec2cb31\" ; }"))))
              (sha256
               (base32
                "01b0wxh0wlxjfz5j8gcwwqhxc6q2kn4njz2fcmzv9fr3xaya5dbp"))
              (file-name (string-append name "-" version))))
    (build-system waf-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'set-rpath-in-LDFLAGS
          ,(ardour-rpath-phase (version-prefix version 1))))
       #:tests? #f ; no check target
       #:python ,python-2))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("aubio" ,aubio)
       ("lrdf" ,lrdf)
       ("boost" ,boost)
       ("atkmm" ,atkmm)
       ("cairomm" ,cairomm)
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
       ("redland" ,redland)
       ("rubberband" ,rubberband)
       ("taglib" ,taglib)
       ("python-rdflib" ,python-rdflib)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (home-page "http://ardour.org")
    (synopsis "Digital audio workstation")
    (description
     "Ardour is a multi-channel digital audio workstation, allowing users to
record, edit, mix and master audio and MIDI projects.  It is targeted at audio
engineers, musicians, soundtrack editors and composers.")
    (license license:gpl2+)))

(define-public ardour
  (package (inherit ardour-3)
    (name "ardour")
    ;; We pick the commit after 4.1 because it fixes a build problem.
    (version "4.1-5aa834e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.ardour.org/ardour/ardour.git")
                    (commit (cadr (string-split version #\-)))))
              (snippet
               ;; Ardour expects this file to exist at build time.  It can be
               ;; created from a git checkout with:
               ;;   ./waf create_stored_revision
               '(call-with-output-file
                    "libs/ardour/revision.cc"
                  (lambda (port)
                    (format port "#include \"ardour/revision.h\"
namespace ARDOUR { const char* revision = \"4.1\" ; }"))))
              (sha256
               (base32
                "0pfmxlscnwyqmgc89cdwrsk1769lr89dirfnpjbxj9zgcns6zqgb"))
              (file-name (string-append name "-" version))))
    (arguments
     (substitute-keyword-arguments (package-arguments ardour-3)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'set-rpath-in-LDFLAGS
                    ,(ardour-rpath-phase (version-prefix version 1)))))))))

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
                "18mdw6nc0vgj6k9rsy0x8w64wvzld0frqshrxxbxfj9qi9843vlc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags
       (list "LV2PEG=ttl2c"
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
    (version "0.0.60")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/calf/calf/"
                    version "/calf-" version ".tar.gz"))
              (sha256
               (base32
                "019fwg00jv217a5r767z7szh7vdrarybac0pr2sk26xp81kibrx9"))))
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

(define-public csound
  (package
    (name "csound")
    (version "6.04")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/csound/csound6/Csound"
                    version "/Csound" version ".tar.gz"))
              (sha256
               (base32
                "1030w38lxdwjz1irr32m9cl0paqmgr02lab2m7f7j1yihwxj1w0g"))))
    (build-system cmake-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("pulseaudio" ,pulseaudio)
       ("libsndfile" ,libsndfile)
       ("liblo" ,liblo)
       ("ladspa" ,ladspa)
       ("jack" ,jack-1)
       ("gettext" ,gnu-gettext)))
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
       (alist-cons-after
        'unpack 'patch-makefile-and-enter-directory
        (lambda _
          (substitute* "libs/Makefile"
            (("/sbin/ldconfig") "true")
            (("^LIBDIR =.*") "LIBDIR = lib\n"))
          (chdir "libs")
          #t)
        (alist-cons-after
         'install
         'install-symlink
         (lambda _
           (symlink "libclalsadrv.so"
                    (string-append (assoc-ref %outputs "out")
                                   "/lib/libclalsadrv.so.2")))
         ;; no configure script
         (alist-delete 'configure %standard-phases)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fftw" ,fftw)))
    (home-page "http://kokkinizita.linuxaudio.org")
    (synopsis "C++ wrapper around the ALSA API")
    (description
     "clalsadrv is a C++ wrapper around the ALSA API simplifying access to
ALSA PCM devices.")
    (license license:gpl2+)))

(define-public fluidsynth
  (package
    (name "fluidsynth")
    (version "1.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/fluidsynth/fluidsynth-"
                    version "/fluidsynth-" version ".tar.gz"))
              (sha256
               (base32
                "070pwb7brdcn1mfvplkd56vjc7lbz4iznzkqvfsakvgbv68k71ah"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (alist-cons-after
        'unpack
        'remove-broken-symlinks
        (lambda _ (delete-file-recursively "m4") #t)
        %standard-phases)))
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
    (license license:gpl2+)))

(define-public faad2
  (package
    (name "faad2")
    (version "2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/faac/faad2-" version ".zip"))
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
       (alist-cons-after
        'unpack 'bootstrap
        (lambda _
          (substitute* "bootstrap" (("\r\n") "\n"))
          (zero? (system* "sh" "bootstrap")))
        %standard-phases)))
    (home-page "http://www.audiocoding.com/faad2.html")
    (synopsis "MPEG-4 and MPEG-2 AAC decoder")
    (description
     "FAAD2 is an MPEG-4 and MPEG-2 AAC decoder supporting LC, Main, LTP, SBR,
PS, and DAB+.")
    (license license:gpl2)))

(define-public faust
  (package
    (name "faust")
    (version "0.9.67")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/faudiostream/faust-" version ".zip"))
              (sha256
               (base32
                "068vl9536zn0j4pknwfcchzi90rx5pk64wbcbd67z32w0csx8xm1"))
              (snippet
               ;; Remove prebuilt library
               '(delete-file "architecture/android/libs/armeabi-v7a/libfaust_dsp.so"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; no "configure" script
         (delete 'configure))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://faust.grame.fr/")
    (synopsis "Signal processing language")
    (description
     "Faust is a programming language for realtime audio signal processing.")
    (license license:gpl2+)))

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
    (version "0.32.3")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/guitarix/guitarix2-"
                   version ".tar.bz2"))
             (sha256
              (base32
               "1ybc5jk7fj6n8qh9ajzl1f6fzdmzab4nwjrh4fsylm94dn1jv0if"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:python ,python-2
       #:configure-flags
       (list
        ;; FIXME: dsp2cc fails for src/faust/tonestack_engl.dsp, so we use the
        ;; generated C++ files rather than compiling them from Faust sources.
        "--no-faust"
        ;; Add the output lib directory to the RUNPATH.
        (string-append "--ldflags=-Wl,-rpath=" %output "/lib"))))
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
       ;;("faust" ,faust) ; dsp2cc fails for src/faust/tonestack_engl.dsp
       ("intltool" ,intltool)
       ("gettext" ,gnu-gettext)
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

(define-public ir
  (package
    (name "ir")
    (version "1.3.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://factorial.hu/system/files/ir.lv2-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1jh2z01l9m4ar7yz0n911df07dygc7n4cl59p7qdjbh0nvkm747g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no "check" target
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       ;; no configure script
       (alist-delete 'configure %standard-phases)))
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
    (home-page "http://factorial.hu/plugins/lv2/ir")
    (synopsis "LV2 convolution reverb")
    (description
     "IR is a low-latency, real-time, high performance signal convolver
especially for creating reverb effects.  It supports impulse responses with 1,
2 or 4 channels, in any soundfile format supported by libsndfile.")
    (license license:gpl2+)))

(define-public jack-1
  (package
    (name "jack")
    (version "0.124.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://jackaudio.org/downloads/jack-audio-connection-kit-"
                   version
                   ".tar.gz"))
             (sha256
              (base32
               "1mk1wnx33anp6haxfjjkfhwbaknfblsvj35nxvz0hvspcmhdyhpb"))))
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
    (version "1.9.10")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/jackaudio/jack2/archive/v"
                   version
                   ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
              (base32
               "03b0iiyk3ng3vh5s8gaqwn565vik7910p56mlbk512bw3dhbdwc8"))))
    (build-system waf-build-system)
    (arguments
     `(#:tests? #f  ; no check target
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
                              "'-Wl,-rpath=" %output "/lib')\n"))))))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("dbus" ,dbus)
       ("expat" ,expat)
       ("libsamplerate" ,libsamplerate)
       ("opus" ,opus)
       ("readline" ,readline)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    ;; Most files are under GPLv2+, but some headers are under LGPLv2.1+
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public jalv
  (package
    (name "jalv")
    (version "1.4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.drobilla.net/jalv-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1f1hcq74n3ziw8bk97mn5a1vgw028dxikv3fchaxd430pbbhqgl9"))))
    (build-system waf-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (inputs
     `(("lv2" ,lv2)
       ("lilv" ,lilv)
       ("suil" ,suil)
       ("gtk" ,gtk+-2)
       ("gtkmm" ,gtkmm-2)
       ("qt" ,qt-4)
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
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.ladspa.org/download/ladspa_sdk_"
                   version
                   ".tgz"))
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
    (home-page "http://ladspa.org")
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
              (uri (string-append
                    "mirror://sourceforge/bs2b/libbs2b-" version ".tar.lzma"))
              (sha256
               (base32
                "1mcc4gjkmphczjybnsrip3gq1f974knzys7x49bv197xk3fn8wdr"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libsndfile" ,libsndfile)))
    (home-page "http://sourceforge.net/projects/bs2b/")
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
             (uri (string-append
                   "mirror://sourceforge/liblo/liblo-"
                   version
                   ".tar.gz"))
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

(define-public lilv
  (package
    (name "lilv")
    (version "0.20.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.drobilla.net/lilv-"
                                 version
                                 ".tar.bz2"))
             (sha256
              (base32
               "0aj2plkx56iar8vzjbq2l7hi7sp0ml99m0h44rgwai2x4vqkk2j2"))))
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
    (version "1.12.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://lv2plug.in/spec/lv2-"
                                 version
                                 ".tar.bz2"))
             (sha256
              (base32
               "1saq0vwqy5zjdkgc5ahs8kcabxfmff2mmg68fiqrkv8hiw9m6jks"))))
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
       #:phases (alist-delete 'configure %standard-phases)))
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
       #:phases (alist-delete 'configure %standard-phases)))
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
                            "/include"))))
    (inputs
     `(("boost" ,boost)
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
    (version "1.15.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://kcat.strangesoft.net/openal-releases/openal-soft-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0mmhdqiyb3c9dzvxspm8h2v8jibhi8pfjxnf6m0wn744y1ia2a8f"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; no check target
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
                  (add-before
                   'configure 'autoconf
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
    (version "0.4.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.drobilla.net/sratom-"
                                 version
                                 ".tar.bz2"))
             (sha256
              (base32
               "080jjiyxjnj7hf25844hd9rb01grvzz1rk8mxcdnakywmspbxfd4"))))
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
    (version "0.8.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.drobilla.net/suil-"
                                 version
                                 ".tar.bz2"))
             (sha256
              (base32
               "1s3adyiw7sa5gfvm5wasa61qa23629kprxyv6w8hbxdiwp0hhxkq"))))
    (build-system waf-build-system)
    (arguments `(#:tests? #f)) ; no check target
    (inputs
     `(("lv2" ,lv2)
       ("gtk+-2" ,gtk+-2)
       ("qt-4" ,qt-4)))
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

Suil currently supports every combination of Gtk 2, Qt 4, and X11.")
    (license license:isc)))

(define-public timidity++
  (package
    (name "timidity++")
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/timidity/TiMidity++-"
                    version ".tar.bz2"))
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
       (alist-cons-after
        'install 'install-config
        (lambda _
          (let ((out (string-append (assoc-ref %outputs "out")
                                    "/etc/timidity")))
            (mkdir-p out)
            (call-with-output-file
                (string-append out "/timidity.cfg")
              (lambda (port)
                (format port (string-append "source "
                                            (assoc-ref %build-inputs "freepats")
                                            "/share/freepats/freepats.cfg"))))))
        %standard-phases)))
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
                    "/attachments/download/1514/vamp-plugin-sdk-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1s986w0mfh1m0870qd7i50hdzayls8kc3shfqf9651jzwdk34lxa"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (alist-cons-after
        'install 'remove-libvamp-hostsdk.la
        (lambda* (#:key outputs #:allow-other-keys)
          ;; https://bugs.launchpad.net/ubuntu/+source/vamp-plugin-sdk/+bug/1253656
          (for-each delete-file
                    (let ((out (assoc-ref outputs "out")))
                      (list (string-append out "/lib/libvamp-sdk.la")
                            (string-append out "/lib/libvamp-hostsdk.la"))))
          #t)
        %standard-phases)))
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
    (version "4.70.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.wavpack.com/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "191h8hv8qk72hfh1crg429i9yq3cminwqb249sy9zadbn1wy7b9c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; wavpack.pc.in lacks path substitution for 'exec_prefix'.
       (list (string-append "--libdir=" %output "/lib"))))
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
                    "mirror://sourceforge/project/modplug-xmms/"
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
    (version "4.3.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xmp/libxmp/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h06091hlpgc6ds4pjmfq8sx4snw7av3nhny180q4pwfyasjb6ny"))))
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
    (version "4.0.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xmp/xmp/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gjylvvmq7ha0nhcjg56qfp0xxpsrcsj7y5r914svd5x1ppmzm5n"))))
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
       (alist-cons-after
        'unpack 'bootstrap
        (lambda _
          (zero? (system* "sh" "bootstrap")))
        %standard-phases)))
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
              (uri (string-append "mirror://sourceforge/sox/sox-"
                                  version ".tar.bz2"))
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
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/soxr/soxr-" version
                       "-Source.tar.xz"))
       (sha256
        (base32 "1hmadwqfpg15vhwq9pa1sl5xslibrjpk6hpq2s9hfmx1s5l6ihfw"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ;no 'check' target
    (home-page "http://sourceforge.net/p/soxr/wiki/Home/")
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
       (uri (string-append
             "mirror://sourceforge/twolame/twolame-" version ".tar.gz"))
       (sha256
        (base32 "0ahiqqng5pidwhj1wzph4vxxgxxgcfa3gl0gywipzx2ii7s35wwq"))))
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
    (version "19.20140130")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.portaudio.com/archives/pa_stable_v"
             (string-map (lambda (c) (if (char=? c #\.) #\_ c)) version)
             ".tgz"))
       (sha256
        (base32 "0mwddk4qzybaf85wqfhxqlf0c5im9il8z03rd4n127k8y2jj9q4g"))
       (patches (list (search-patch "portaudio-audacity-compat.patch")))))
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
       (alist-cons-after
        'unpack 'autoreconf
        (lambda _
          (zero? (system* "autoreconf" "-vif")))
        %standard-phases)
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
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/qsynth/qsynth-" version ".tar.gz"))
       (sha256
        (base32 "08kyn6cl755l9i1grzjx8yi3f8mgiz4gx0hgqad1n0d8yz85087b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no "check" phase
    (inputs
     `(("qt" ,qt)
       ("fluidsynth" ,fluidsynth)))
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
       (sha256
        (base32 "1wzs40c0k5zpkmm5ffl6c17xmr399sxli7ys0fbb9ib0fd334knx"))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("jack" ,jack-2)
       ("ao" ,ao)
       ("libsamplerate" ,libsamplerate)
       ("openal" ,openal)
       ("portaudio" ,portaudio)
       ("pulseaudio" ,pulseaudio)))
    (arguments
     '(#:phases
       (alist-replace
        'configure
        (lambda* (#:key outputs #:allow-other-keys)
          (setenv "CC" "gcc")
          (zero?
           (system* "./configure"
                    (string-append "--prefix=" (assoc-ref outputs "out")))))
        %standard-phases)
       ;; No 'check' target.
       #:tests? #f))
    (home-page "http://themaister.net/rsound.html")
    (synopsis "Networked audio system")
    (description
     "RSound allows you to send audio from an application and transfer it
directly to a different computer on your LAN network.  It is an audio daemon
with a much different focus than most other audio daemons.")
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
              (sha256
               (base32
                "14qrnczhp5mbwhky64il7kxc4hl1mmh495v60va7i2qnhasr6zmz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (alist-cons-after
        'unpack 'patch-makefile-and-enter-directory
        (lambda _
          (substitute* "libs/Makefile"
            (("ldconfig") "true")
            (("^LIBDIR =.*") "LIBDIR = lib\n"))
          (chdir "libs") #t)
        (alist-cons-after
         'install
         'install-symlink
         (lambda _
           (symlink "libzita-convolver.so"
                    (string-append (assoc-ref %outputs "out")
                                   "/lib/libzita-convolver.so.3")))
         ;; no configure script
         (alist-delete 'configure %standard-phases)))))
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
       (alist-cons-after
        'unpack 'patch-makefile-and-enter-directory
        (lambda _
          (substitute* "libs/Makefile"
            (("ldconfig") "true")
            (("^LIBDIR =.*") "LIBDIR = lib\n"))
          (chdir "libs")
          #t)
        (alist-cons-after
         'install
         'install-symlink
         (lambda _
           (symlink "libzita-alsa-pcmi.so"
                    (string-append (assoc-ref %outputs "out")
                                   "/lib/libzita-alsa-pcmi.so.0")))
         ;; no configure script
         (alist-delete 'configure %standard-phases)))))
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
