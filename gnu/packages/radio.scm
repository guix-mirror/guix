;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2019, 2020 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Charlie Ritter <chewzerita@posteo.net>
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

(define-module (gnu packages radio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt))

(define-public liquid-dsp
  (package
    (name "liquid-dsp")
    (version "1.3.2")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/jgaeddert/liquid-dsp.git")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32 "1n6dbg13q8ga5qhg1yiszwly4jj0rxqr6f1xwm9waaly5z493xsd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)))
    (inputs
     `(("fftw" ,fftw)
       ("fftwf" ,fftwf)))
    (home-page "https://liquidsdr.org")
    (synopsis "Signal processing library for software-defined radios")
    (description
     "Liquid DSP is a @dfn{digital signal processing} (DSP) library designed
specifically for software-defined radios on embedded platforms.  The aim is to
provide a lightweight DSP library that does not rely on a myriad of external
dependencies or proprietary and otherwise cumbersome frameworks.  All signal
processing elements are designed to be flexible, scalable, and dynamic,
including filters, filter design, oscillators, modems, synchronizers, complex
mathematical operations, and much more.")
    (license license:expat)))

(define-public rtl-sdr
  (package
    (name "rtl-sdr")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "git://git.osmocom.org/rtl-sdr.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lmvsnb4xw4hmz6zs0z5ilsah5hjz29g1s0050n59fllskqr3b8k"))))
    (build-system cmake-build-system)
    (inputs
     `(("libusb" ,libusb)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags '("-DDETACH_KERNEL_DRIVER=ON")
       #:tests? #f)) ; No tests
    (home-page "https://osmocom.org/projects/sdr/wiki/rtl-sdr")
    (synopsis "Software defined radio driver for Realtek RTL2832U")
    (description "DVB-T dongles based on the Realtek RTL2832U can be used as a
cheap software defined radio, since the chip allows transferring the raw I/Q
samples to the host.  @code{rtl-sdr} provides drivers for this purpose.")
    (license license:gpl2+)))

(define-public chirp
  (package
    (name "chirp")
    (version "20181205")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://trac.chirp.danplanet.com/chirp_daily/daily-"
                           version "/chirp-daily-" version ".tar.gz"))
       (sha256
        (base32
         "1cp280b95j39xaxs50zn55jigg7pyfpm9n098hmsyxrplqn8z43c"))))
    (build-system python-build-system)
    (inputs
     `(("python2-libxml2" ,python2-libxml2)
       ("python2-pygtk" ,python2-pygtk)
       ("python2-pyserial" ,python2-pyserial)))
    (arguments
     `(#:python ,python-2))
    (home-page "https://chirp.danplanet.com")
    (synopsis "Cross-radio programming tool")
    (description "Chirp is a cross-radio programming tool.  It supports a
growing list of radios across several manufacturers and allows transferring of
memory contents between them.")
    (license (list license:gpl3+
                   license:lgpl3+)))) ; chirp/elib_intl.py

(define-public aptdec
  (package
    (name "aptdec")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/csete/aptdec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hf0zb51qc6fyhdjxyij1n3vgwnw3cwksc3r11szbhkml14qjnzk"))))
    (build-system gnu-build-system)
    (inputs
     `(("libpng" ,libpng)
       ("libsndfile" ,libsndfile)))
    (arguments
     `(#:make-flags (list "CC=gcc")
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "atpdec" (string-append out "/bin")))
             #t)))))
    (home-page "https://github.com/csete/aptdec")
    (synopsis "NOAA Automatic Picture Transmission (APT) decoder")
    (description "Aptdec decodes Automatic Picture Transmission (APT) images.
These are medium resolution images of the Earth transmitted by, among other
satellites, the POES NOAA weather satellite series.  These transmissions are
on a frequency of 137 MHz.  They can be received using an inexpensive antenna
and a dedicated receiver.")
    (license license:gpl2+)))

(define-public redsea
  (package
    (name "redsea")
    (version "0.18")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/windytan/redsea")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1y96g0ra2krjb2kypm8s5gdfia45yci4f36klsvyzg8d53v5cwhn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The configure.ac file does not explicitly link against libiconv
         ;; except on Mac OS, causing the build to fail. This phase comments
         ;; out the original AC_SUBST macro (located inside a conditional) and
         ;; adds an explicit use of it underneath, so that libiconv is always
         ;; linked against.
         (add-after 'unpack 'patch-libiconv
           (lambda _
             (substitute* "configure.ac"
               (("^ +AC_SUBST")
                "# AC_SUBST")
               (("esac")
                "esac\nAC_SUBST([ICONV], [\"-liconv\"])"))
             #t)))))
    (inputs
     `(("libiconv" ,libiconv)
       ("libsndfile" ,libsndfile)
       ("liquid-dsp" ,liquid-dsp)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (home-page "https://github.com/windytan/redsea")
    (synopsis "Lightweight RDS to JSON decoder")
    (description "redsea is a lightweight command-line @dfn{FM Radio Data
System} (FM-RDS) decoder.  Redsea can be used with any RTL-SDR USB radio stick
with the rtl_fm tool, or any other @dfn{software-defined radio} (SDR) via
csdr, for example.  It can also decode raw ASCII bitstream, the hex format
used by RDS Spy, and audio files containing @dfn{multiplex} signals (MPX).")
    (license license:expat)))

(define-public gnuradio
  (package
    (name "gnuradio")
    (version "3.8.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gnuradio.org/releases/gnuradio/"
                           "gnuradio-" version ".tar.xz"))
       (sha256
        (base32 "0aw55gf5549b0fz2qdi7vplcmaf92bj34h40s34b2ycnqasv900r"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled volk to use the shared one.
           (delete-file-recursively "volk")
           #t))))
    (build-system cmake-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("git" ,git-minimal)
       ("ghostscript" ,ghostscript)
       ("orc" ,orc)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python-cheetah" ,python-cheetah)
       ("python-mako" ,python-mako)
       ("python-pyzmq" ,python-pyzmq)
       ("python-scipy" ,python-scipy)
       ("python-sphinx" ,python-sphinx)
       ("swig" ,swig)
       ("texlive" ,(texlive-union (list texlive-amsfonts
                                        texlive-latex-amsmath
                                        ;; TODO: Add newunicodechar.
                                        texlive-latex-graphics)))
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("cairo" ,cairo)
       ("codec2" ,codec2)
       ("cppzmq" ,cppzmq)
       ("fftwf" ,fftwf)
       ("gmp" ,gmp)
       ("gsl" ,gsl)
       ("gsm" ,gsm)
       ("gtk+" ,gtk+)
       ("jack" ,jack-1)
       ("log4cpp" ,log4cpp)
       ("pango" ,pango)
       ("portaudio" ,portaudio)
       ("python-click" ,python-click)
       ("python-click-plugins" ,python-click-plugins)
       ("python-lxml" ,python-lxml)
       ("python-numpy" ,python-numpy)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-pyqt" ,python-pyqt)
       ("python-pyyaml" ,python-pyyaml)
       ("qtbase" ,qtbase)
       ("qwt" ,qwt)
       ("volk" ,volk)
       ("zeromq" ,zeromq)))
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils)
                  (ice-9 match))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build glib-or-gtk-build-system)
                           (guix build python-build-system))
       #:configure-flags
       '("-DENABLE_INTERNAL_VOLK=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((qwt (assoc-ref inputs "qwt")))
               (substitute* "cmake/Modules/FindQwt.cmake"
                 (("/usr/include")
                  (string-append qwt "/include"))
                 (("/usr/lib")
                  (string-append qwt "/lib"))
                 (("qwt6-\\$\\{QWT_QT_VERSION\\}")
                  "qwt")))
             (substitute* "cmake/Modules/GrPython.cmake"
               (("dist-packages")
                "site-packages"))
             (substitute* '("gr-vocoder/swig/vocoder_swig.i"
                            "gr-vocoder/include/gnuradio/vocoder/codec2.h"
                            "gr-vocoder/include/gnuradio/vocoder/freedv_api.h")
               (("<codec2/")
                "<"))
             #t))
         (add-before 'check 'set-test-environment
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HOME" "/tmp")
             (system (string-append (assoc-ref inputs "xorg-server")
                                    "/bin/Xvfb :1 &"))
             (setenv "DISPLAY" ":1")
             #t))
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'wrap-python 'wrap-glib-or-gtk
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'wrap-glib-or-gtk 'wrap-with-GI_TYPELIB_PATH
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (paths (map (match-lambda
                                 ((output . directory)
                                  (let ((girepodir (string-append
                                                    directory
                                                    "/lib/girepository-1.0")))
                                    (if (file-exists? girepodir)
                                        girepodir
                                        #f))))
                               inputs)))
               (wrap-program (string-append out "/bin/gnuradio-companion")
                 `("GI_TYPELIB_PATH" ":" prefix ,(filter identity paths))))
             #t)))))
    (synopsis "Toolkit for software-defined radios")
    (description
     "GNU Radio is a development toolkit that provides signal processing blocks
to implement software radios.  It can be used with external RF hardware to
create software-defined radios, or without hardware in a simulation-like
environment.")
    (home-page "https://www.gnuradio.org")
    (license license:gpl3+)))

(define-public gnuradio-osmosdr
  (package
    (name "gnuradio-osmosdr")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.osmocom.org/gr-osmosdr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rdx7fa0xiq0qmgrrbby7z1bblmqhl9qh9jqpsznzxkx91f17ypd"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("git" ,git-minimal)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python-mako" ,python-mako)
       ("python-six" ,python-six)
       ("swig" ,swig)))
    (inputs
     `(("boost" ,boost)
       ("fftwf" ,fftwf)
       ("gmp" ,gmp)
       ("gnuradio" ,gnuradio)
       ("log4cpp" ,log4cpp)
       ;; TODO: Add more drivers.
       ("rtl-sdr" ,rtl-sdr)
       ("volk" ,volk)))
    (synopsis "GNU Radio block for interfacing with various radio hardware")
    (description "This is a block for GNU Radio allowing to use a common API
to access different radio hardware.")
    (home-page "https://osmocom.org/projects/gr-osmosdr/wiki/GrOsmoSDR")
    (license license:gpl3+)))

(define-public libosmo-dsp
  (package
    (name "libosmo-dsp")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.osmocom.org/libosmo-dsp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00plihnpym1gkfpflah8il9463qxzm9kx2f07jyvbkszpj8viq5g"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bash-minimal" ,bash-minimal)
       ("doxygen" ,doxygen)
       ("git" ,git-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("texlive" ,(texlive-union (list texlive-amsfonts
                                        texlive-latex-amsmath
                                        ;; TODO: Add newunicodechar.
                                        texlive-latex-graphics)))))
    (inputs
     `(("fftwf" ,fftwf)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "git-version-gen"
               (("/bin/sh")
                (string-append (assoc-ref inputs "bash")
                               "/bin/bash")))
             #t)))))
    (synopsis "DSP primitives for SDR")
    (description
     "This a C-language library for common DSP (Digital Signal Processing)
primitives for SDR (Software Defined Radio).")
    (home-page "https://osmocom.org/projects/libosmo-dsp")
    (license license:gpl2+)))

(define-public gnuradio-iqbalance
  (package
    (name "gnuradio-iqbalance")
    (version "0.38.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.osmocom.org/gr-iqbal")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ksagwz05p3b0702q7ljq7013xmp0ijp30my9z6s3p7ja8dj42s3"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python-numpy" ,python-numpy)
       ("python-six" ,python-six)
       ("swig" ,swig)))
    (inputs
     `(("boost" ,boost)
       ("fftwf" ,fftwf)
       ("gmp" ,gmp)
       ("gnuradio" ,gnuradio)
       ("libosmo-dsp" ,libosmo-dsp)
       ("log4cpp" ,log4cpp)
       ("volk" ,volk)))
    (synopsis "GNU Radio block to correct IQ imbalance")
    (description
     "This is a GNU Radio block to correct IQ imbalance in quadrature
receivers.  It's composed of two main block:
@itemize
@item Fix: Given a phase and amplitude error, it will correct a complex signal.
@item Optimize: Attempts to auto-detect the phase and amplitude error to feed
to the fix block above.
@end itemize")
    (home-page "https://git.osmocom.org/gr-iqbal/")
    (license license:gpl3+)))

(define-public gqrx
  (package
    (name "gqrx")
    (version "2.12.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/csete/gqrx.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00alf3q6y313xpx6p7v43vqsphd2x4am4q362lw21bcy9wc4jidw"))))
    (build-system qt-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("fftwf" ,fftwf)
       ("gmp" ,gmp)
       ("gnuradio" ,gnuradio)
       ("gnuradio-iqbalance" ,gnuradio-iqbalance)
       ("gnuradio-osmosdr" ,gnuradio-osmosdr)
       ("jack" ,jack-1)
       ("log4cpp" ,log4cpp)
       ("portaudio" ,portaudio)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("volk" ,volk)))
    (arguments
     `(#:tests? #f)) ; No tests
    (synopsis "Software defined radio receiver")
    (description "Gqrx is a software defined radio (SDR) receiver implemented
using GNU Radio and the Qt GUI toolkit.")
    (home-page "https://gqrx.dk/")
    (license license:gpl3+)))

(define-public fldigi
  (package
    (name "fldigi")
    (version "4.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.w1hkj.com/files/fldigi/fldigi-"
                           version ".tar.gz"))
       (sha256
        (base32 "1y62xn1pim38ibaf2mbl8b7aq20jdaac6lgggb9r402w9bj5b196"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fltk" ,fltk)
       ("libpng" ,libpng)
       ("libsamplerate" ,libsamplerate)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxft" ,libxft)
       ("portaudio" ,portaudio)
       ("pulseaudio" ,pulseaudio)))
    (synopsis "Software modem for amateur radio use")
    (description
     "Fldigi is a software modem for amateur radio use.  It is a sound card
based program that is used for both transmitting and receiving data by
connecting the microphone and headphone connections of a computer to some radio
hardware.")
    (home-page "http://www.w1hkj.com/")
    (license license:gpl3+)))
