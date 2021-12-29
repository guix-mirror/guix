;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
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

(define-module (gnu packages pulseaudio)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages web)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public libsndfile
  (package
    (name "libsndfile")
    (version "1.0.30")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/erikd/libsndfile"
                                 "/releases/download/v" version
                                 "/libsndfile-" version ".tar.bz2"))
             (sha256
              (base32
               "06k1wj3lwm7vf21s8yqy51k6nrkn9z610bj1gxb618ag5hq77wlx"))
             (modules '((ice-9 textual-ports) (guix build utils)))
             (snippet
              '(begin
                 ;; Remove carriage returns (CRLF) to prevent bogus
                 ;; errors from bash like "$'\r': command not found".
                 (let ((data (call-with-input-file
                                 "tests/pedantic-header-test.sh.in"
                               (lambda (port)
                                 (string-join
                                  (string-split (get-string-all port)
                                                #\return))))))
                   (call-with-output-file "tests/pedantic-header-test.sh.in"
                     (lambda (port) (format port data))))

                 ;; While at it, fix hard coded executable name.
                 (substitute* "tests/test_wrapper.sh.in"
                   (("^/usr/bin/env") "env"))
                 #t))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list flac libogg libvorbis opus))
    (native-inputs
     (list pkg-config python))
    (home-page "http://www.mega-nerd.com/libsndfile/")
    (synopsis "Reading and writing files containing sampled sound")
    (description
     "Libsndfile is a C library for reading and writing files containing
sampled sound (such as MS Windows WAV and the Apple/SGI AIFF format) through
one standard library interface.

It was designed to handle both little-endian (such as WAV) and
big-endian (such as AIFF) data, and to compile and run correctly on
little-endian (such as Intel and DEC/Compaq Alpha) processor systems as well
as big-endian processor systems such as Motorola 68k, Power PC, MIPS and
SPARC.  Hopefully the design of the library will also make it easy to extend
for reading and writing new sound file formats.")
    (license l:gpl2+)))

(define-public libsamplerate
  (package
    (name "libsamplerate")                     ; aka. Secret Rabbit Code (SRC)
    (version "0.1.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.mega-nerd.com/SRC/libsamplerate-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1ha46i0nbibq0pl0pjwcqiyny4hj8lp1bnl4dpxm64zjw9lb2zha"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config automake)) ;For up to date 'config.guess' and 'config.sub'.
    (propagated-inputs
     (list libsndfile fftw))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-configure
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             ;; Replace outdated config.sub and config.guess:
             (with-directory-excursion "Cfg"
               (for-each (lambda (file)
                           (install-file (string-append
                                          (assoc-ref
                                           (or native-inputs inputs) "automake")
                                          "/share/automake-"
                                          ,(version-major+minor
                                            (package-version automake))
                                          "/" file) "."))
                         '("config.sub" "config.guess")))
             #t)))))
    (home-page "http://www.mega-nerd.com/SRC/index.html")
    (synopsis "Audio sample rate conversion library")
    (description
     "Secret Rabbit Code (aka. libsamplerate) is a Sample Rate Converter for
audio.  One example of where such a thing would be useful is converting audio
from the CD sample rate of 44.1kHz to the 48kHz sample rate used by DAT
players.

SRC is capable of arbitrary and time varying conversions; from downsampling by
a factor of 256 to upsampling by the same factor.  Arbitrary in this case means
that the ratio of input and output sample rates can be an irrational
number.  The conversion ratio can also vary with time for speeding up and
slowing down effects.

SRC provides a small set of converters to allow quality to be traded off
against computation cost.  The current best converter provides a
signal-to-noise ratio of 145dB with -3dB passband extending from DC to 96% of
the theoretical best bandwidth for a given pair of input and output sample
rates.")
    (license l:bsd-2)))

(define-public pulseaudio
  (package
    (name "pulseaudio")
    (version "15.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://freedesktop.org/software/pulseaudio/releases/"
                   name "-" version ".tar.xz"))
             (sha256
              (base32
               "1851rg4h6sjwanvd294hn52z321rc6vbs4gbfrlw53597dx8h2x4"))
             (modules '((guix build utils)))
             (snippet
              ;; Disable console-kit support by default since it's deprecated
              ;; anyway.
              '(begin
                 (substitute* "src/daemon/default.pa.in"
                   (("load-module module-console-kit" all)
                    (string-append "#" all "\n")))
                 #t))
             (patches (search-patches
                       "pulseaudio-fix-mult-test.patch"
                       "pulseaudio-longer-test-timeout.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list "-Doss-output=disabled"
               "-Dlocalstatedir=/var"
               (string-append "-Dudevrulesdir="
                              out "/lib/udev/rules.d")
               ;; Ensure the RUNPATH contains all installed library locations.
               (string-append "-Dc_link_args=-Wl,-rpath="
                              out "/lib/pulseaudio:"
                              out "/lib:"
                              out "/lib/pulse-" ,version "/modules")))
       #:phases (modify-phases %standard-phases
                 (add-before 'check 'pre-check
                   (lambda _
                     ;; 'tests/lock-autospawn-test.c' wants to create a file
                     ;; under ~/.config/pulse.
                     (setenv "HOME" (getcwd))
                     ;; 'thread-test' needs more time on hydra and on slower
                     ;; machines, so we set the default timeout to 120 seconds.
                     (setenv "CK_DEFAULT_TIMEOUT" "120")
                     #t)))))
    (inputs
     (list alsa-lib
           bluez
           sbc
           speexdsp
           libsndfile
           jack-1 ; For routing the output to jack.
           dbus
           glib
           libltdl
           fftwf
           avahi
           webrtc-audio-processing
           ;; For the optional X11 modules.
           libice
           libsm
           libxcb
           libxtst
           elogind
           eudev))         ;for the detection of hardware audio devices
    (native-inputs
     `(("check" ,check)
       ("doxygen" ,doxygen)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("m4" ,m4)
       ("perl" ,perl)
       ("perl-xml-parser" ,perl-xml-parser)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; 'libpulse*.la' contain `-ltdb' and `-lcap', so propagate them.
     (list libcap tdb))
    (home-page "http://www.pulseaudio.org/")
    (synopsis "Sound server")
    (description
     "PulseAudio is a sound server.  It is basically a proxy for your sound
applications.  It allows you to do advanced operations on your sound data as
it passes between your application and your hardware.  Things like
transferring the audio to a different machine, changing the sample format or
channel count and mixing several sounds into one are easily achieved using a
sound server.")

    ;; PulseAudio is LGPLv2+, but some of the optional dependencies (GNU dbm,
    ;; FFTW, etc.) are GPL'd, so the result is effectively GPLv2+.  See
    ;; 'LICENSE' for details.
    (license l:gpl2+)))

(define-public pavucontrol
  (package
    (name "pavucontrol")
    (version "5.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://freedesktop.org/software/pulseaudio/pavucontrol/pavucontrol-"
                   version
                   ".tar.xz"))
             (sha256
              (base32
               "0yjfiwpaydh5s8v3l78dhwhbsmcl1xsq3p8rvz80m9zinp1p4ayf"))))
    (build-system glib-or-gtk-build-system)
    (inputs
     (list adwaita-icon-theme ;hard-coded theme
           gtkmm-3
           json-glib
           libcanberra
           pulseaudio))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://www.freedesktop.org/software/pulseaudio/pavucontrol/")
    (synopsis "PulseAudio volume control")
    (description
     "PulseAudio Volume Control (pavucontrol) provides a GTK+
graphical user interface to connect to a PulseAudio server and
easily control the volume of all clients, sinks, etc.")
    (license l:gpl2+)))

(define-public ponymix
  (package
    (name "ponymix")
    (version "5")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/falconindy/ponymix/")
                    (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "08yp7fprmzm6px5yx2rvzri0l60bra5h59l26pn0k071a37ks1rb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There is no test suite.
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "DESTDIR=" out)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda _
             (substitute* "Makefile"
               (("/usr") ""))))
         (delete 'configure)))) ; There's no configure phase.
    (inputs
     (list pulseaudio))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/falconindy/ponymix")
    (synopsis "Console-based PulseAudio mixer")
    (description "Ponymix is a PulseAudio mixer and volume controller with a
command-line interface.  In addition, it is possible to use named sources and
sinks.")
    (license l:expat)))

(define-public pulsemixer
  (package
    (name "pulsemixer")
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/GeorgeFilipkin/pulsemixer")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jagx9zmz5pfsld8y2rj2kqg6ww9f6vqiawfy3vhqc49x3xx92p4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pulse (assoc-ref inputs "pulseaudio")))
               (substitute* "pulsemixer"
                 (("libpulse.so.0")
                  (string-append pulse "/lib/libpulse.so.0")))
               #t))))))
    (inputs
     (list pulseaudio))
    (home-page "https://github.com/GeorgeFilipkin/pulsemixer/")
    (synopsis "Command-line and curses mixer for PulseAudio")
    (description "Pulsemixer is a PulseAudio mixer with command-line and
curses-style interfaces.")
    (license l:expat)))

(define-public pulseaudio-dlna
  ;; The last release was in 2016; use a more recent commit.
  (let ((commit "4472928dd23f274193f14289f59daec411023ab0")
        (revision "1"))
    (package
      (name "pulseaudio-dlna")
      (version (git-version "0.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/masmu/pulseaudio-dlna")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1dfn7036vrq49kxv4an7rayypnm5dlawsf02pfsldw877hzdamqk"))))
      (build-system python-build-system)
      (arguments `(#:python ,python-2))
      (inputs
       (list python2-chardet
             python2-dbus
             python2-docopt
             python2-futures
             python2-pygobject
             python2-lxml
             python2-netifaces
             python2-notify2
             python2-protobuf
             python2-psutil
             python2-requests
             python2-pyroute2
             python2-setproctitle
             python2-zeroconf))
      (home-page "https://github.com/masmu/pulseaudio-dlna")
      (synopsis "Stream audio to DLNA/UPnP and Chromecast devices")
      (description "This lightweight streaming server brings DLNA/UPnP and
Chromecast support to PulseAudio.  It can stream your current PulseAudio
playback to different UPnP devices (UPnP Media Renderers, including Sonos
devices and some Smart TVs) or Chromecasts in your network.  You should also
install one or more of the following packages alongside pulseaudio-dlna:

@itemize
@item ffmpeg - transcoding support for multiple codecs
@item flac - FLAC transcoding support
@item lame - MP3 transcoding support
@item opus-tools - Opus transcoding support
@item sox - WAV transcoding support
@item vorbis-tools - Vorbis transcoding support
@end itemize")
      (license l:gpl3+))))

(define-public pamixer
  (package
    (name "pamixer")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cdemoulins/pamixer")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d5arjbsh3q9z693pi2rq553ai9b18iz36ss7q8ff29m0hf62lzd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'install 'mkdir-bin
           (lambda _
             (mkdir-p (string-append (assoc-ref %outputs "out") "/bin")))))))
    (inputs
     (list boost pulseaudio))
    (home-page "https://github.com/cdemoulins/pamixer")
    (synopsis "PulseAudio command line mixer")
    (description
     "pamixer is like amixer but for PulseAudio, allowing easy control of the
volume levels of the sinks (get, set, decrease, increase, toggle mute, etc).")
    (license l:gpl3+)))

(define-public pasystray
  (package
    (name "pasystray")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/christophgysin/pasystray")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xx1bm9kimgq11a359ikabdndqg5q54pn1d1dyyjnrj0s41168fk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'remove-bootstrap.sh
           (lambda _
             ;; Interferes with the bootstrap phase.
             (delete-file "bootstrap.sh")
             #t)))))
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list avahi gtk+ libnotify libx11 pulseaudio))
    (home-page "https://github.com/christophgysin/pasystray")
    (synopsis "PulseAudio controller for the system tray")
    (description "@command{pasystray} enables control of various
PulseAudio server settings from the X11 system tray.  See the project
README.md for a detailed list of features.")
    (license l:lgpl2.1+)))

(define-public paprefs
  (package
    (name "paprefs")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/pulseaudio/"
                           name "/" name "-" version ".tar.xz"))
       (sha256
        (base32
         "189z5p20hk0xv9vwvym293503j4pwl03xqk9hl7cl6dwgv0l7wkf"))))
    (build-system meson-build-system)
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list gtkmm-3 pulseaudio))
    (home-page "https://freedesktop.org/software/pulseaudio/paprefs/")
    (synopsis "Simple GTK based configuration dialog for the PulseAudio sound
server")
    (description "@command{paprefs} is a simple GTK based configuration
dialog for the PulseAudio sound server.  Note that this program can
only configure local servers, and requires that a special module
module-gsettings is loaded in the sound server.")
    (license l:gpl2)))

(define-public rnnoise
  (package
    (name "rnnoise")
    (version "0.91")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/werman/noise-suppression-for-voice")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11pwisbcks7g0mdgcrrv49v3ci1l6m26bbb7f67xz4pr1hai5dwc"))))
    (build-system cmake-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (inputs
     (list ;; TODO: Package VST to build the corresponding plugin.
           pulseaudio))
    (home-page "https://github.com/werman/noise-suppression-for-voice")
    (synopsis "Real-time Noise suppression plugin based on Xiph's RNNoise")
    (description "The plugin is meant to suppress a wide range of noise
origins: computer fans, office, crowd, airplane, car, train, construction.

Mild background noise is always suppressed, loud sounds, like
clicking of mechanical keyboard, are suppressed while there is no voice
however they are only reduced in volume when voice is present.

The plugin is made to work with 1 or 2 channels (ladspa plugin),
16 bit, 48000 Hz audio input.")
    (license l:gpl3)))

(define-public noisetorch
  (package
    (name "noisetorch")
    (version "0.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lawl/NoiseTorch")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "115sq4waq048bv82lnq5sblf62m50yvyakx7x06qq359v7qf5ji1"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/lawl/NoiseTorch"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'symlink-rnnoise
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/github.com/lawl/NoiseTorch"
               (let ((dir "librnnoise_ladspa/bin/ladspa")
                     (rnnoise (assoc-ref inputs "rnnoise")))
                 (mkdir-p dir)
                 (symlink (string-append rnnoise "/lib/ladspa/librnnoise_ladspa.so")
                          (string-append dir "/librnnoise_ladspa.so"))))
             #t))
         (add-after 'unpack 'gen-version.go
           (lambda _
             (with-directory-excursion "src/github.com/lawl/NoiseTorch"
               (substitute* "main.go"
                 (("//go:generate go run scripts/embedversion\\.go") ""))
               (with-output-to-file "version.go"
                 (lambda ()
                   (format #t "package main~%~%var version=~s~&" ,version))))
             #t))
         (add-after 'unpack 'disable-update-check.go
           (lambda _
             (with-directory-excursion "src/github.com/lawl/NoiseTorch"
               (substitute* "main.go"
                 ((".*updateCheck.*") "")))
             #t))
         (add-before 'build 'go-generate
           (lambda _
             (with-directory-excursion "src/github.com/lawl/NoiseTorch"
               (invoke "go" "generate")))))))
    (inputs
     (list rnnoise))
    (home-page "https://github.com/lawl/NoiseTorch")
    (synopsis "Real-time microphone noise suppression")
    (description "NoiseTorch creates a virtual PulseAudio microphone that
suppresses noise, in any application.  Use whichever conferencing or VOIP
application you like and simply select the NoiseTorch Virtual Microphone as
input to torch the sound of your mechanical keyboard, computer fans, trains
and the likes.")
    (license l:gpl3)))
