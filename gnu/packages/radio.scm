;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019, 2020 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2019, 2020 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020 Charlie Ritter <chewzerita@posteo.net>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 João Pedro Simas <jpsimas@gmail.com>
;;; Copyright © 2021 Jack Hill <jackhill@jackhill.us>
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
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt))

(define-public libfec
  ;; Use commit to get compilation fixes that are not in a release yet.
  (let ((commit "9750ca0a6d0a786b506e44692776b541f90daa91")
        (revision "1"))
    (package
      (name "libfec")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/quiet/libfec")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0i6jhrdswr1wglyb9h39idpz5v9z13yhidvlbj34vxpyngrkhlvs"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
         #:test-target "test_all"))
      (home-page "https://github.com/quiet/libfec")
      (synopsis "Forward error correction algorithms library")
      (description
       "This package provides a set of functions that implement several popular
@dfn{forward error correction} (FEC) algorithms and several low-level routines
useful in modems implemented with @dfn{digital signal processing} (DSP).")
      (license license:lgpl2.1))))

(define-public liquid-dsp
  (package
    (name "liquid-dsp")
    (version "1.3.2")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/jgaeddert/liquid-dsp")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32 "1n6dbg13q8ga5qhg1yiszwly4jj0rxqr6f1xwm9waaly5z493xsd"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("fftwf" ,fftwf)
       ("libfec" ,libfec)))
    (arguments
     `(;; For reproducibility, disable use of SSE3, SSE4.1, etc.
       #:configure-flags '("--enable-simdoverride")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'delete-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/lib/libliquid.a")))))))
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
             (url "https://git.osmocom.org/rtl-sdr/")
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
     `(#:configure-flags '("-DDETACH_KERNEL_DRIVER=ON"
                           "-DINSTALL_UDEV_RULES=ON")
       #:tests? #f ; No tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("DESTINATION \"/etc/udev/")
                (string-append "DESTINATION \""
                               (assoc-ref outputs "out")
                               "/lib/udev/")))
             #t)))))
    (home-page "https://osmocom.org/projects/sdr/wiki/rtl-sdr")
    (synopsis "Software defined radio driver for Realtek RTL2832U")
    (description "DVB-T dongles based on the Realtek RTL2832U can be used as a
cheap software defined radio, since the chip allows transferring the raw I/Q
samples to the host.  @code{rtl-sdr} provides drivers for this purpose.

The default Linux driver managing DVB-T dongles as TV devices doesn't work for
SDR purposes and clashes with this package.  Therefore you must prevent the
kernel from loading it automatically by adding the following line to your
system configuration:

@lisp
(kernel-arguments '(\"modprobe.blacklist=dvb_usb_rtl28xxu\"))
@end lisp

To install the rtl-sdr udev rules, you must extend 'udev-service-type' with
this package.  E.g.: @code{(udev-rules-service 'rtl-sdr rtl-sdr)}")
    (license license:gpl2+)))

(define-public airspyhf
  (package
    (name "airspyhf")
    (version "1.6.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/airspy/airspyhf")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n699i5a9fzzhf80fcjlqq6p2a013rzlwmwv4nmwfafy6c8cr924"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libusb" ,libusb)))
    (arguments
     '(#:configure-flags '("-DINSTALL_UDEV_RULES=ON")
       #:tests? #f ; No tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "tools/CMakeLists.txt"
               (("DESTINATION \"/etc/udev/")
                (string-append "DESTINATION \""
                               (assoc-ref outputs "out")
                               "/lib/udev/")))))
         (add-after 'fix-paths 'fix-udev-rules
           (lambda _
             (substitute* "tools/52-airspyhf.rules"
               ;; The plugdev group does not exist; use dialout as in
               ;; the hackrf package.
               (("GROUP=\"plugdev\"")
                "GROUP=\"dialout\"")))))))
    (home-page "https://github.com/airspy/airspyhf")
    (synopsis "Software defined radio driver for Airspy HF+")
    (description
     "This package provides the driver and utilities for controlling the Airspy
HF+ Software Defined Radio (SDR) over USB.

To install the airspyhf udev rules, you must extend @code{udev-service-type}
with this package.  E.g.: @code{(udev-rules-service 'airspyhf airspyhf)}")
    (license license:bsd-3)))

(define-public soapysdr
  (package
    (name "soapysdr")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pothosware/SoapySDR")
             (commit (string-append "soapy-sdr-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dy25zxk7wmg7ik82dx7h3bbbynvalbz1dxsl7kgm3374yxhnixv"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python)
       ("swig" ,swig)))
    (native-search-paths
     (list (search-path-specification
            (variable "SOAPY_SDR_PLUGIN_PATH")
            (files (list (string-append "lib/SoapySDR/modules"
                                        (version-major+minor version)))))))
    (home-page "https://github.com/pothosware/SoapySDR/wiki")
    (synopsis "Vendor and platform neutral SDR support library")
    (description
     "SoapySDR is a library designed to support several kinds of software
defined radio hardware devices with a common API.")
    (license license:boost1.0)))

(define-public soapyairspyhf
  (package
    (name "soapyairspyhf")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pothosware/SoapyAirspyHF")
             (commit (string-append "soapy-airspyhf-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04krqinglgkjvx7klqik6yn8rb4mlpwzb6zvnmvm7szqci2agggz"))))
    (build-system cmake-build-system)
    (inputs
     `(("airspyhf" ,airspyhf)
       ("soapysdr" ,soapysdr)))
    (arguments
     `(#:tests? #f))  ; No test suite
    (home-page "https://github.com/pothosware/SoapyAirspyHF/wiki")
    (synopsis "SoapySDR Airspy HF+ module")
    (description "This package provides Airspy HF+ devices support to the
SoapySDR library.")
    (license license:expat)))

(define-public soapyhackrf
  ;; Use commit because some fixes are not in a release yet
  ;; (last release was in 2018).
  (let ((commit "8d2e7beebb4c451609cb0cee236fa4d20a0e28b1")
        (revision "2"))
    (package
      (name "soapyhackrf")
      (version (git-version "0.3.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pothosware/SoapyHackRF")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0l5890a240i1fan88jjdxaqswk3as410nlrv12a698fy9npqh4w4"))))
      (build-system cmake-build-system)
      (inputs
       `(("hackrf" ,hackrf)
         ("soapysdr" ,soapysdr)))
      (arguments
       `(#:tests? #f))  ; No test suite
      (home-page "https://github.com/pothosware/SoapyHackRF/wiki")
      (synopsis "SoapySDR HackRF module")
      (description
       "This package provides HackRF devices support to the SoapySDR library.")
      (license license:expat))))

(define-public soapyrtlsdr
  (package
    (name "soapyrtlsdr")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pothosware/SoapyRTLSDR")
             (commit (string-append "soapy-rtl-sdr-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dlnryj6k20pk7w7v4v13y099r7ikhvlzbgzgphmi5cxkdv0shrd"))))
    (build-system cmake-build-system)
    (inputs
     `(("rtl-sdr" ,rtl-sdr)
       ("soapysdr" ,soapysdr)))
    (arguments
     `(#:tests? #f))  ; No test suite
    (home-page "https://github.com/pothosware/SoapyRTLSDR/wiki")
    (synopsis "SoapySDR RTL-SDR module")
    (description
     "This package provides RTL-SDR devices support to the SoapySDR library.")
    (license license:expat)))

(define-public chirp
  (package
    (name "chirp")
    (version "20201121")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://trac.chirp.danplanet.com/chirp_daily/daily-"
                           version "/chirp-daily-" version ".tar.gz"))
       (sha256
        (base32 "092jryb1jn9li6zj243awv6piz1lhghqsm4phrz7j0rgqf76dy4n"))))
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
  ;; No release since 2013, use commit directly.
  (let ((commit "5f91799637d93dfe7791caa7e9a6683050c4f8f3")
        (revision "1"))
    (package
      (name "aptdec")
      (version (git-version "1.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Xerbo/aptdec")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0i7vkjjrq392gs9qaibr7j3v4hijqqg8458dn21dwh16ncrvr9bp"))))
      (build-system cmake-build-system)
      (inputs
       `(("libpng" ,libpng)
         ("libsndfile" ,libsndfile)))
      (arguments
       `(#:tests? #f))  ; no tests
      (home-page "https://github.com/Xerbo/aptdec")
      (synopsis "NOAA Automatic Picture Transmission (APT) decoder")
      (description "Aptdec decodes Automatic Picture Transmission (APT) images.
These are medium resolution images of the Earth transmitted by, among other
satellites, the POES NOAA weather satellite series.  These transmissions are
on a frequency of 137 MHz.  They can be received using an inexpensive antenna
and a dedicated receiver.")
      (license license:gpl2+))))

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
    (version "3.9.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnuradio/gnuradio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01wyqazrpphmb0fl69j93k0w4vm4d1l4177m1fyg7qx8hzia0aaq"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("ghostscript" ,ghostscript)
       ("js-mathjax" ,js-mathjax)
       ("orc" ,orc)
       ("pkg-config" ,pkg-config)
       ("pybind11" ,pybind11)
       ("python-cheetah" ,python-cheetah)
       ("python-mako" ,python-mako)
       ("python-pyzmq" ,python-pyzmq)
       ("python-scipy" ,python-scipy)
       ("python-sphinx" ,python-sphinx)
       ("texlive" ,(texlive-union (list texlive-amsfonts/patched
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
       ("libsndfile" ,libsndfile)
       ("log4cpp" ,log4cpp)
       ("pango" ,pango)
       ("portaudio" ,portaudio)
       ("python" ,python)
       ("python-click" ,python-click)
       ("python-click-plugins" ,python-click-plugins)
       ("python-lxml" ,python-lxml)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("python-pyqt" ,python-pyqt-without-qtwebkit)
       ("python-pyyaml" ,python-pyyaml)
       ("qtbase" ,qtbase-5)
       ("qwt" ,qwt)
       ("sdl" ,sdl)
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
       (list (string-append "-DMATHJAX2_ROOT="
                            (assoc-ref %build-inputs "js-mathjax")
                            "/share/javascript/mathjax"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-source-writable
           (lambda _
             ;; The test_add and test_newmod open(sources, "w") for some reason.
             (for-each make-file-writable
                       (find-files "." ".*"))))
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
             (substitute* '("gr-vocoder/include/gnuradio/vocoder/codec2.h"
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
         (replace 'check
           (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
             (invoke "ctest" "-j" (if parallel-tests?
                                      (number->string (parallel-job-count))
                                      "1")
                     "--output-on-failure"
                     ;;disable broken tests
                     "-E" (string-join
                           '(;; https://github.com/gnuradio/gnuradio/issues/3871
                             "qa_header_payload_demux"
                             ;; https://github.com/gnuradio/gnuradio/issues/4348
                             "qa_packet_headerparser_b")
                           "|"))))
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
    (native-search-paths
     ;; Variables required to find third-party plugins at runtime.
     (list (search-path-specification
            (variable "GRC_BLOCKS_PATH")
            (files '("share/gnuradio/grc/blocks")))
           (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor
                                         (package-version python))
                                        "/site-packages"))))))
    (synopsis "Toolkit for software-defined radios")
    (description
     "GNU Radio is a development toolkit that provides signal processing blocks
to implement software radios.  It can be used with external RF hardware to
create software-defined radios, or without hardware in a simulation-like
environment.")
    (home-page "https://www.gnuradio.org")
    (license license:gpl3+)))

(define-public gr-osmosdr
  ;; No tag for version supporting Gnuradio 3.9; use commit.
  (let ((commit "a100eb024c0210b95e4738b6efd836d48225bd03")
        (revision "0"))
    (package
      (name "gr-osmosdr")
      (version (git-version "0.2.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.osmocom.org/gr-osmosdr")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1pk5gnyznfyy510lbqzg9ijcb1fnhmn547n24aiqyrxd6i6vv1ki"))))
      (build-system cmake-build-system)
      (native-inputs
       `(("doxygen" ,doxygen)
         ("pkg-config" ,pkg-config)
         ("pybind11" ,pybind11)
         ("python-mako" ,python-mako)
         ("python-six" ,python-six)))
      (inputs
       `(("airspyhf" ,airspyhf)
         ("boost" ,boost)
         ("fftwf" ,fftwf)
         ("gmp" ,gmp)
         ("gnuradio" ,gnuradio)
         ("hackrf" ,hackrf)
         ("libsndfile" ,libsndfile)
         ("log4cpp" ,log4cpp)
         ("python" ,python)
         ("python-numpy" ,python-numpy)
         ("python-pyqt" ,python-pyqt)
         ("rtl-sdr" ,rtl-sdr)
         ("soapysdr" ,soapysdr)
         ("volk" ,volk)))
      (arguments
       `(#:modules ((guix build cmake-build-system)
                    ((guix build python-build-system) #:prefix python:)
                    (guix build utils))
         #:imported-modules (,@%cmake-build-system-modules
                             (guix build python-build-system))
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-python
             (assoc-ref python:%standard-phases 'wrap)))))
      (synopsis "GNU Radio block for interfacing with various radio hardware")
      (description "This is a block for GNU Radio allowing to use a common API
to access different radio hardware.")
      (home-page "https://osmocom.org/projects/gr-osmosdr/wiki/GrOsmoSDR")
      (license license:gpl3+))))
(deprecated-package "gnuradio-osmosdr" gr-osmosdr)

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
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("texlive" ,(texlive-union (list texlive-amsfonts/patched
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

(define-public gr-iqbal
  ;; No tag for version supporting Gnuradio 3.9; use commit.
  (let ((commit "fbee239a6fb36dd2fb564f6e6a0d393c4bc844db")
        (revision "0"))
    (package
      (name "gr-iqbal")
      (version (git-version "0.38.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.osmocom.org/gr-iqbal")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12p193ngcs65nd3lynry119nhv40mikamqkw37wdln7lawx3nw7p"))))
      (build-system cmake-build-system)
      (native-inputs
       `(("doxygen" ,doxygen)
         ("pkg-config" ,pkg-config)
         ("pybind11" ,pybind11)
         ("python" ,python)
         ("python-numpy" ,python-numpy)
         ("python-six" ,python-six)))
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
      (license license:gpl3+))))
(deprecated-package "gnuradio-iqbalance" gr-iqbal)

(define-public gr-satellites
  (package
    (name "gr-satellites")
    (version "4.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/daniestevez/gr-satellites")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01p9cnwjxas3pkqr9m5fnrgm45cji0sfdqqa51hzy7izx9vgzaf8"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("pybind11" ,pybind11)
       ("python-six" ,python-six)))
    (inputs
     `(("boost" ,boost)
       ("gmp" ,gmp)
       ("gnuradio" ,gnuradio)
       ("log4cpp" ,log4cpp)
       ("python" ,python)
       ("python-construct" ,python-construct)
       ("python-numpy" ,python-numpy)
       ("python-pyaml" ,python-pyaml)
       ("python-pyzmq" ,python-pyzmq)
       ("python-requests" ,python-requests)
       ("volk" ,volk)))
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-test-environment
           (lambda _
             (setenv "HOME" "/tmp")))
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap)))))
    (synopsis "GNU Radio decoders for several Amateur satellites")
    (description
     "@code{gr-satellites} is a GNU Radio out-of-tree module encompassing
a collection of telemetry decoders that supports many different Amateur
satellites.")
    (home-page "https://github.com/daniestevez/gr-satellites")
    (license (list license:asl2.0
                   license:gpl3+
                   license:lgpl2.1))))

(define-public gqrx
  (package
    (name "gqrx")
    (version "2.14.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/csete/gqrx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m4ncydihz4n4i80c252vk3c5v672yab1jv85n6ndn7a92xv3ilq"))))
    (build-system qt-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("boost" ,boost)
       ("fftwf" ,fftwf)
       ("gmp" ,gmp)
       ("gnuradio" ,gnuradio)
       ("gr-iqbal" ,gr-iqbal)
       ("gr-osmosdr" ,gr-osmosdr)
       ("jack" ,jack-1)
       ("libsndfile" ,libsndfile)
       ("log4cpp" ,log4cpp)
       ("portaudio" ,portaudio)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase-5)
       ("qtsvg" ,qtsvg)
       ("volk" ,volk)))
    (arguments
     `(#:tests? #f))                    ; no tests
    (synopsis "Software defined radio receiver")
    (description "Gqrx is a software defined radio (SDR) receiver implemented
using GNU Radio and the Qt GUI toolkit.")
    (home-page "https://gqrx.dk/")
    (license license:gpl3+)))

(define-public fldigi
  (package
    (name "fldigi")
    (version "4.1.20")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/fldigi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y43241s3p8qzn7x6x28v5v2bf934riznj14bb7m6k6vgd849qzl"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fltk" ,fltk)
       ("eudev" ,eudev)
       ("hamlib" ,hamlib)
       ("libpng" ,libpng)
       ("libsamplerate" ,libsamplerate)
       ("libusb" ,libusb)
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

(define-public flrig
  (package
    (name "flrig")
    (version "1.3.52")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/flrig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vxn1wy5b2zfq20k93rfgq34m1nd3mxd74h8l98f90d85fhcqggy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("fltk" ,fltk)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxft" ,libxft)))
    (synopsis "Radio transceiver control program")
    (description
     "Flrig is a transceiver control program for amateur radio use.
It provides computer aided control of various radios using a serial
or USB connection.")
    (home-page "http://www.w1hkj.com/")
    (license license:gpl3+)))

(define-public flamp
  (package
    (name "flamp")
    (version "2.2.05")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/flamp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ll2wbhyh1sb4iqsypwrd118mrgw3vbsdbz442qhk4r6l8kjzblq"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("fltk" ,fltk)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxft" ,libxft)))
    (synopsis "Tool for AMP file transfer")
    (description
     "FLAMP is a program for transferring files by radio waves using AMP
(Amateur Multicast Protocol).")
    (home-page "http://www.w1hkj.com/")
    (license license:gpl3+)))

(define-public flwrap
  (package
    (name "flwrap")
    (version "1.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/flwrap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xkhr82smfr7wpb9xl05wf7bz3vi2mr4xkcr2s8v6mblhgsdhqwg"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("fltk" ,fltk)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxft" ,libxft)))
    (synopsis "File encapsulation program")
    (description
     "Flwrap is a software utility for amateur radio use.  Its purpose is to
encapsulate both text and binary files in a way that allows them to be
transmitted over any of several digital modes and verified at the receipt end
for correctness.")
    (home-page "http://www.w1hkj.com/")
    (license license:gpl3+)))

(define-public hackrf
  (package
    (name "hackrf")
    (version "2021.03.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mossmann/hackrf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12fkgimjy5ia291c1rn4y59pn9r5wdvz5x9z5xc8zr1xr96iyhfs"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DUDEV_RULES_GROUP=dialout"
             (string-append "-DUDEV_RULES_PATH="
                            (assoc-ref %outputs "out")
                            "/lib/udev/rules.d"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enter-source-directory
           (lambda _
             (chdir "host")
             #t))
         (add-after 'install 'delete-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/lib/libhackrf.a"))
             #t))
         (add-before 'install-license-files 'leave-source-directory
           (lambda _
             (chdir "..")
             #t)))
       #:tests? #f)) ; no test suite
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("libusb" ,libusb)))
    (home-page "https://greatscottgadgets.com/hackrf/")
    (synopsis "User-space library and utilities for HackRF SDR")
    (description
     "Command line utilities and a C library for controlling the HackRF
Software Defined Radio (SDR) over USB.  Installing this package installs the
userspace hackrf utilities and C library.  To install the hackrf udev rules,
you must extend 'udev-service-type' with this package.  E.g.:
@code{(udev-rules-service 'hackrf hackrf #:groups '(\"dialout\"))}.")
    (license license:gpl2)))

(define-public hamlib
  (package
    (name "hamlib")
    (version "4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Hamlib/Hamlib/releases/download/"
             version "/hamlib-" version ".tar.gz"))
       (sha256
        (base32 "1m8gb20i8ga6ndnnw187ry1h4z8wx27v1hl7c610r6ky60pv4072"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("lua" ,lua)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)
       ("swig" ,swig)
       ("tcl" ,tcl)))
    (inputs
     `(("gd" ,gd)
       ("libusb" ,libusb)
       ("libxml2" ,libxml2)
       ("readline" ,readline)))
    (arguments
     `(#:configure-flags '("--disable-static"
                           "--with-lua-binding"
                           "--with-python-binding"
                           "--with-tcl-binding"
                           "--with-xml-support")))
    (synopsis "Tools and API to control radios")
    (description
     "The Ham Radio Control Library (Hamlib) is a project to provide programs
with a consistent Application Programming Interface (API) for controlling the
myriad of radios and rotators available to amateur radio and communications
users.")
    (home-page "https://hamlib.github.io/")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define wsjtx-hamlib
  ;; Fork of hamlib with custom patches used by wsjtx.
  (package
    (inherit hamlib)
    (name "wsjtx-hamlib")
    (version "2.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/u/bsomervi/hamlib.git")
             (commit (string-append "wsjtx-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m4yzjcqs7a1w4lghyyckpkiy96jxdjijddxarqr3a37cl2rz23j"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("texinfo" ,texinfo)
       ,@(package-native-inputs hamlib)))
    (arguments
     `(#:configure-flags '("--disable-static"
                           "--with-lua-binding"
                           "--with-python-binding"
                           "--with-tcl-binding"
                           "--with-xml-support")))))

(define-public tlf
  (package
    (name "tlf")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tlf/tlf")
             (commit (string-append "tlf-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xpgs4k27pjd9mianfknknp6mf34365bcp96wrv5xh4dhph573rj"))
       (patches
        (search-patches "tlf-support-hamlib-4.2+.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--enable-fldigi-xmlrpc")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cmocka" ,cmocka)
       ("glib" ,glib)
       ("hamlib" ,hamlib)
       ("libusb" ,libusb) ;`Requires.private: libusb-1.0` in hamlib pkg-config
       ("ncurses" ,ncurses)
       ("xmlrpc-c" ,xmlrpc-c)))
    (home-page "https://tlf.github.io/")
    (synopsis "Amateur radio contest logging for the terminal")
    (description "TLF is a @acronym{Text User Interface, TUI} amateur radio
contest logging program.  It integrates with radios supported by hamlib and
other ham radio programs like fldigi.  Many contests are supported including:

@itemize
@item CQWW (SO, M/S and M/M)
@item WPX (SO, M/S and M/M)
@item ARRL Sweepstakes (SO, M/S )
@item EU SPRINT
@item EUHFC
@item ARRL-DX (both sides)
@item ARRL-FD
@item ARRL 10m
@item ARRL 160m
@item Region1 field day
@item SP DX contest
@item PACC (both sides)
@item NRAU - scandinavian
@item Wysiwyg mults mode (per band or per contest)
@item WAEDX
@end itemize

It also supports connecting to DX clusters, log synchronization with other TLF
instances over the network, and general QSO and DXpedition logging.")
    (license license:gpl2+)))

(define-public wsjtx
  (package
    (name "wsjtx")
    (version "2.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/wsjt/wsjtx.git")
             (commit (string-append "wsjtx-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mdr4l7zii08615yn7z91spnvnqm5i9390bra9lz3aqyxrsiim91"))))
    (build-system qt-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("ruby-asciidoctor" ,ruby-asciidoctor)))
    (inputs
     `(("boost" ,boost)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("hamlib" ,wsjtx-hamlib)
       ("libusb" ,libusb)
       ("qtbase" ,qtbase-5)
       ("qtmultimedia" ,qtmultimedia)
       ("qtserialport" ,qtserialport)))
    (arguments
     `(#:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'work-around-runtime-bug
           (lambda _
             ;; Some of the programs in this package fail to find symbols
             ;; in libm at runtime. Adding libm manually at the end of the
             ;; library lists when linking the programs seems to help.
             ;; TODO: find exactly what is wrong in the way the programs
             ;; are built.
             (substitute* "CMakeLists.txt"
               (("target_link_libraries \\((.*)\\)" all libs)
                (string-append "target_link_libraries (" libs " m)")))
             #t)))))
    (synopsis "Weak-signal ham radio communication program")
    (description
     "WSJT-X implements communication protocols or modes called FT4, FT8,
JT4, JT9, JT65, QRA64, ISCAT, MSK144, and WSPR, as well as one called Echo for
detecting and measuring your own radio signals reflected from the Moon.  These
modes were all designed for making reliable, confirmed QSOs under extreme
weak-signal conditions.")
    (home-page "https://www.physics.princeton.edu/pulsar/k1jt/wsjtx.html")
    (license license:gpl3)))

(define-public js8call
  (package
    (inherit wsjtx)
    (name "js8call")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://files.js8call.com/" version
                           "/js8call-" version ".tgz"))
       (sha256
        (base32 "149sjwc4zg6ckgq26af93p4fxappa4k9dh7rdy67g8ajfjad4cd8"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled boost to use the shared one.
           (delete-file-recursively "boost")
           #t))))
    (build-system qt-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)
       ("qttools" ,qttools)
       ("ruby-asciidoctor" ,ruby-asciidoctor)))
    (inputs
     `(("boost" ,boost)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("hamlib" ,wsjtx-hamlib)
       ("libusb" ,libusb)
       ("qtbase" ,qtbase-5)
       ("qtmultimedia" ,qtmultimedia)
       ("qtserialport" ,qtserialport)))
    (arguments
     `(#:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("DESTINATION /usr/share")
                (string-append "DESTINATION "
                               (assoc-ref outputs "out")
                               "/share")))
             #t))
         (add-after 'fix-paths 'work-around-runtime-bug
           (lambda _
             ;; Some of the programs in this package fail to find symbols
             ;; in libm at runtime. Adding libm manually at the end of the
             ;; library lists when linking the programs seems to help.
             ;; TODO: find exactly what is wrong in the way the programs
             ;; are built.
             (substitute* "CMakeLists.txt"
               (("target_link_libraries \\((.*)\\)" all libs)
                (string-append "target_link_libraries (" libs " m)")))
             #t))
         (add-after 'unpack 'fix-hamlib
           (lambda _
             (substitute* "CMake/Modules/Findhamlib.cmake"
               (("set \\(ENV\\{PKG_CONFIG_PATH\\}.*\\)")
                "set (__pc_path $ENV{PKG_CONFIG_PATH})
  list (APPEND __pc_path \"${__hamlib_pc_path}\")
  set (ENV{PKG_CONFIG_PATH} \"${__pc_path}\")"))
             (substitute* "HamlibTransceiver.hpp"
               (("#ifdef JS8_USE_LEGACY_HAMLIB")
                "#if 1"))
             #t)))))
    (synopsis "Weak-signal ham radio communication program")
    (description
     "JS8Call is a software using the JS8 digital mode (a derivative of the FT8
mode) providing weak signal keyboard to keyboard messaging to amateur radio
operators.")
    (home-page "http://js8call.com/")
    (license license:gpl3)))

(define-public xnec2c
  (package
    (name "xnec2c")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.5b4az.org/pkg/nec2/xnec2c/xnec2c-"
                           version ".tar.bz2"))
       (sha256
        (base32 "1myvlkfybb2ha8l0h96ca3iz206zzy9z5iizm0sbab2zzp78n1r9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-makefile
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("Makefile.am" "Makefile.in")
               ;; The DESTDIR variable does not get replaced the prefix
               ;; in the final Makefile, so let's do here.
               (("\\$\\(DESTDIR\\)/usr")
                (assoc-ref outputs "out")))
             #t))
         (add-after 'fix-makefile 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Increase the max length of the path to the glade file,
             ;; so that the '/gnu/store/...' path can fit in.
             (substitute* '("src/shared.c" "src/shared.h")
               (("char xnec2c_glade\\[64\\];")
                "char xnec2c_glade[256];"))
             ;; Fix hard coded references to '/usr/...'.
             (substitute* '("src/geom_edit.c" "src/main.c")
               (("\"/usr")
                (string-append "\"" (assoc-ref outputs "out"))))
             #t)))))
    (synopsis "Antenna modeling software")
    (description
     "Xnec2c is a GTK3-based graphical version of nec2c, a translation to the
C language of NEC2, the FORTRAN Numerical Electromagnetics Code commonly used
for antenna simulation and analysis.  It can be used to define the geometry of
an antenna, and then plot the radiation pattern or frequency-related data like
gain and standing wave ratio.")
    (home-page "http://www.5b4az.org/")
    (license license:gpl3+)))

(define-public dump1090
  (package
    (name "dump1090")
    (version "5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flightaware/dump1090")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fckfcgypmplzl1lidd04jxiabczlfx9mv21d6rbsfknghsjpn03"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("hackrf" ,hackrf)
       ("libusb" ,libusb)
       ("ncurses" ,ncurses)
       ("rtl-sdr" ,rtl-sdr)))
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             "BLADERF=no")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (install-file "dump1090" bin)
               (install-file "view1090" bin)
               #t))))))
    (synopsis "Mode S decoder for rtl-sdr devices")
    (description
     "Dump1090 is a Mode S decoder specifically designed for rtl-sdr devices.
It can be used to decode the ADS-B signals that planes emit to indicate
their position, altitude, speed, etc.")
    (home-page "https://github.com/flightaware/dump1090")
    (license license:gpl2+)))

(define-public rtl-433
  (package
    (name "rtl-433")
    (version "21.05")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/merbanan/rtl_433")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f60nvahsplv1yszacc49mlbcnacgs1nwhdf8y9srmzg08xrfnfk"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libusb" ,libusb)
       ("openssl" ,openssl)
       ("rtl-sdr" ,rtl-sdr)
       ("soapysdr" ,soapysdr)))
    (synopsis "Decoder for radio transmissions in ISM bands")
    (description
     "This is a generic data receiver, mainly for decoding radio transmissions
from devices on the 433 MHz, 868 MHz, 315 MHz, 345 MHz and 915 MHz ISM bands.")
    (home-page "https://github.com/merbanan/rtl_433")
    (license license:gpl2+)))

(define-public multimon-ng
  (package
    (name "multimon-ng")
    (version "1.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/EliasOenal/multimon-ng")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01716cfhxfzsab9zjply9giaa4nn4b7rm3p3vizrwi7n253yiwm2"))))
    (build-system cmake-build-system)
    (inputs
     `(("libx11" ,libx11)
       ("pulseaudio" ,pulseaudio)))
    (arguments
     '(#:tests? #f))                    ; no test suite
    (home-page "https://github.com/EliasOenal/multimon-ng")
    (synopsis "Decoder for digital radio transmission modes")
    (description "Multimon-ng can decode several digital radio transmission
modes:
@itemize
@item POCSAG512, POCSAG1200, POCSAG2400
@item FLEX
@item EAS
@item UFSK1200, CLIPFSK, AFSK1200, AFSK2400, AFSK2400_2, AFSK2400_3
@item HAPN4800
@item FSK9600
@item DTMF
@item ZVEI1, ZVEI2, ZVEI3, DZVEI, PZVEI
@item EEA, EIA, CCIR
@item MORSE CW
@item X10
@end itemize")
    (license license:gpl2+)))

(define-public nanovna-saver
  (package
    (name "nanovna-saver")
    (version "0.3.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NanoVNA-Saver/nanovna-saver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h5k402wjlj7xjniggwf0x7a5srlgglc2x4hy6lz6c30zwa7z8fm"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-cython" ,python-cython)))
    (inputs
     `(("python-numpy" ,python-numpy)
       ("python-pyqt" ,python-pyqt)
       ("python-pyserial" ,python-pyserial)
       ("python-scipy" ,python-scipy)))
    (arguments
     '(#:tests? #f))
    (home-page "https://github.com/NanoVNA-Saver/nanovna-saver")
    (synopsis "GUI for NanoVNA devices")
    (description
     "NanoVNA-Saver is a tool for reading, displaying and saving data from the
NanoVNA vector network analyzers.")
    (license license:gpl3+)))

(define-public qsstv
  (package
    (name "qsstv")
    (version "9.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://users.telenet.be/on4qz/qsstv/downloads/"
                           "qsstv_" version ".tar.gz"))
       (sha256
        (base32 "0f9hx6sy418cb23fadll298pqbc5l2lxsdivi4vgqbkvx7sw58zi"))))
    (build-system qt-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("hamlib" ,hamlib)
       ("openjpeg" ,openjpeg)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase-5)
       ("v4l-utils" ,v4l-utils)))
    (arguments
     `(#:tests? #f  ; No test suite.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-newer-hamlib-support
           (lambda _
             (substitute* "qsstv/rig/rigcontrol.cpp"
               (("FILPATHLEN")
                "HAMLIB_FILPATHLEN"))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX=" (assoc-ref outputs "out")))
             #t)))))
    (home-page "http://users.telenet.be/on4qz/qsstv/")
    (synopsis "Program for receiving and transmitting SSTV and HAMDRM")
    (description
     "QSSTV is a program for receiving and transmitting SSTV and HAMDRM
(sometimes called DSSTV).  It is compatible with most of MMSSTV and EasyPal.")
    (license (list license:gpl2+
                   license:qwt1.0))))

(define-public direwolf
  (package
    (name "direwolf")
    (version "1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wb2osz/direwolf")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xmz64m02knbrpasfij4rrq53ksxna5idxwgabcw4n2b1ig7pyx5"))))
    (build-system cmake-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("hamlib" ,hamlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "conf/CMakeLists.txt"
               (("DESTINATION /etc")
                (string-append "DESTINATION "
                               (assoc-ref outputs "out")
                               "/etc"))))))))
    (home-page "https://github.com/wb2osz/direwolf")
    (synopsis "TNC for Amateur Packet Radio")
    (description
     "Dire Wolf is a Terminal Node Controller (TNC) for Amateur Packet Radio.
It can perform as:
@itemize
@item APRS GPS tracker,
@item Digipeater,
@item Internet gateway (IGate)
@item APRStt gateway
@end itemize\n")
    (license license:gpl2+)))

(define-public aldo
  (package
    (name "aldo")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/aldo/aldo-" version ".tar.bz2"))
       (sha256
        (base32 "14lzgldqzbbzydsy1cai3wln3hpyj1yhj8ji3wygyzr616fq9f7i"))))
    (build-system gnu-build-system)
    (inputs
     `(("ao" ,ao)))
    (home-page "https://www.nongnu.org/aldo/")
    (synopsis "Morse code tutor")
    (description
     "Aldo is a morse code learning tool providing four type of training
methods:

@itemize
@item Classic exercice,
@item Koch method,
@item Read from file,
@item Callsign exercice.
@end itemize\n")
    (license license:gpl3+)))

(define-public unixcw
  (package
    (name "unixcw")
    (version "3.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/unixcw/unixcw-"
                           version ".tar.gz"))
       (sha256
        (base32 "15wriwv91583kmmyijbzam3dpclzmg4qjyfzjv5f75x9b0gqabxm"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ncurses" ,ncurses)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase-5)))
    (arguments
     `(#:configure-flags '("--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("configure"
                            "src/config.h.in"
                            "src/cwcp/Makefile.am"
                            "src/cwcp/Makefile.in")
               (("-lcurses")
                "-lncurses"))
             (substitute* "src/libcw/libcw_pa.c"
               (("libpulse-simple.so" all)
                (string-append (assoc-ref inputs "pulseaudio")
                               "/lib/" all))))))))
    (home-page "http://unixcw.sourceforge.net/")
    (synopsis "Morse code library and programs")
    (description
     "@code{unixcw} is a project providing the libcw library and a set of
programs using the library: cw, cwgen, cwcp and xcwcp.  The programs are
intended for people who want to learn receiving and sending morse code.")
    (license license:gpl2+)))

(define-public gnuais
  (package
    (name "gnuais")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rubund/gnuais")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rik5fdfslszdn3yvj769jzmnv9pirzf76ki33bjjzk7nkabbnlm"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("mariadb-dev" ,mariadb "dev")
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("curl" ,curl)
       ("gtk+" ,gtk+)
       ("libsoup" ,libsoup-minimal)
       ("mariadb-lib" ,mariadb "lib")
       ("osm-gps-map" ,osm-gps-map)
       ("pulseaudio" ,pulseaudio)))
    (arguments
     `(#:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/cfgfile.c"
               (("/usr/share/")
                (string-append (assoc-ref outputs "out") "/share/"))))))))
    (home-page "http://gnuais.sourceforge.net/")
    (synopsis "AIS message demodulator and decoder")
    (description
     "This program contains algorithms to demodulate and decode AIS (Automatic
Identification System) messages sent by ships and coast stations.")
    (license license:gpl2+)))

(define-public kappanhang
  (package
    (name "kappanhang")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nonoo/kappanhang")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ycy8avq5s7zspfi0d9klqcwwkpmcaz742cigd7pmcnbbhspcicp"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nonoo/kappanhang"
       #:install-source? #f))
    (inputs
     `(("go-github-com-akosmarton-papipes",go-github-com-akosmarton-papipes)
       ("go-github-com-fatih-color" ,go-github-com-fatih-color)
       ("go-github-com-google-goterm" ,go-github-com-google-goterm)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github-com-mesilliac-pulse-simple"
        ,go-github-com-mesilliac-pulse-simple)
       ("go-github-com-pborman-getopt" ,go-github-com-pborman-getopt)
       ("go-go-uber-org-multierr" ,go-go-uber-org-multierr)
       ("go-go-uber-org-zap" ,go-go-uber-org-zap)))
    (home-page "https://github.com/nonoo/kappanhang")
    (synopsis "Client for Icom RS-BA1 server")
    (description
     "Kappanhang remotely opens audio channels and a serial port to an Icom
RS-BA1 server.  The application is mainly developed for connecting to the Icom
IC-705 transceiver, which has built-in WiFi and RS-BA1 server.

Compatible hardware/software:
@itemize
@item Icom RS-BA1 server software,
@item Icom IC-705
@item Icom IC-9700
@end itemize\n")
    (license license:expat)))

(define-public dream
  (package
    (name "dream")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/drm/dream/" version
                           "/dream-" version "-svn808.tar.gz"))
       (sha256
        (base32 "01dv6gvljz64zrjbr08mybr9aicvpq2c6qskww46lngdjyhk8xs1"))))
    (build-system qt-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("faad2" ,faad2)
       ("fftw" ,fftw)
       ("libsndfile" ,libsndfile)
       ("libpcap" ,libpcap)
       ("opus" ,opus)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase-5)
       ("qtsvg" ,qtsvg)
       ("qtwebkit" ,qtwebkit)
       ("qwt" ,qwt)
       ("speexdsp" ,speexdsp)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "dream.pro"
               (("target\\.path = /usr/bin")
                (string-append "target.path = "
                               (assoc-ref outputs "out") "/bin"))
               (("documentation\\.path = /usr/share/man/man1")
                (string-append "documentation.path = "
                               (assoc-ref outputs "out")
                               "/share/man/man1"))
               (("/usr/include/pulse/")
                (string-append (assoc-ref inputs "pulseaudio")
                               "/include/pulse/"))
               (("/usr/include/sndfile\\.h")
                (string-append (assoc-ref inputs "libsndfile")
                               "/include/sndfile.h"))
               (("/usr/include/opus/")
                (string-append (assoc-ref inputs "opus")
                               "/include/opus/"))
               (("/usr/include/speex/")
                (string-append (assoc-ref inputs "speexdsp")
                               "/include/speex/"))
               (("/usr/include/qwt/")
                (string-append (assoc-ref inputs "qwt")
                               "/include/qwt/"))
               (("\\$\\$OUT_PWD/include/neaacdec\\.h")
                (string-append (assoc-ref inputs "faad2")
                               "/include/neaacdec.h")))))
         (replace 'configure
           (lambda _
             (invoke "qmake"))))))
    (home-page "https://sourceforge.net/projects/drm/")
    (synopsis "Digital Radio Mondiale receiver")
    (description
     "Dream is a software implementation of a Digital Radio Mondiale (DRM)
receiver.")
    (license license:gpl2+)))

(define-public welle-io
  (package
    (name "welle-io")
    (version "2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/albrechtl/welle.io")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xl1lanw0xgmgks67dbfb2h52jxnrd1i2zik56v0q8dwsr7f0daw"))))
    (build-system qt-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("faad2" ,faad2)
       ("fftwf" ,fftwf)
       ("lame" ,lame)
       ("libusb" ,libusb)
       ("mpg123" ,mpg123)
       ("rtl-sdr" ,rtl-sdr)
       ("qtbase" ,qtbase-5)
       ("qtcharts" ,qtcharts)
       ("qtdeclarative" ,qtdeclarative)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtmultimedia" ,qtmultimedia)
       ("qtquickcontrols2" ,qtquickcontrols2)
       ("soapysdr" ,soapysdr)))
    (arguments
     `(#:configure-flags '("-DRTLSDR=ON"
                           "-DSOAPYSDR=ON")
       #:tests? #f))
    (home-page "https://www.welle.io/")
    (synopsis "DAB and DAB+ software radio")
    (description
     "@code{welle.io} is a Digital Audio Broadcasting (DAB and DAB+) software
defined radio with support for rtl-sdr.")
    (license license:gpl2+)))

(define-public csdr
  ;; No release since 2017, use commit directly.
  (let ((commit "6ef2a74206887155290a54c7117636f66742f858")
        (revision "1"))
    (package
      (name "csdr")
      (version (git-version "0.15" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ha7ilm/csdr")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ic35130lf66lk3wawgc5bcg711l7chv9al1hzdc1xrmq9qf9hri"))))
      (build-system gnu-build-system)
      (inputs
       `(("fftwf" ,fftwf)))
      (arguments
       `(#:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
               ;; Don't print summary of SIMD optimized functions.
               "PARSEVECT=no")
         #:tests? #f  ; No check phase
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "Makefile"
                 (("PARAMS_MISC = -Wno-unused-result" all)
                  ;; The 'validate-runpath' phase fails without this.
                  (string-append
                   all " -Wl,-rpath=" (assoc-ref outputs "out") "/lib"))
                 (("PARAMS_SIMD =.*")
                  ;; Disable to make reproducibility and cross-compilation work.
                  "")
                 (("gcc ")
                  ,(string-append (cc-for-target) " "))
                 (("g\\+\\+ ")
                  ,(string-append (cxx-for-target) " ")))))
           (add-before 'install 'make-installation-directories
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (mkdir-p (string-append out "/bin"))
                 (mkdir-p (string-append out "/lib"))))))))
      (home-page "https://github.com/ha7ilm/csdr")
      (synopsis "DSP for software defined radio")
      (description
       "This package includes the @code{libcsdr} library of
@acronym{DSP, Digital Signal Processing} functions for
@acronym{SDRs, Software Defined Radios}, and the @code{csdr} command line
program that can be used to build simple signal processing flow graphs.")
      (license license:gpl3+))))

(define-public serialdv
  (package
    (name "serialdv")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/f4exb/serialDV")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d88h2wjhf79nisiv96bq522hkbknzm88wsv0q9k33mzmrwnrx93"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))  ; No test suite.
    (home-page "https://github.com/f4exb/serialDV")
    (synopsis "Audio interface for AMBE3000 based devices")
    (description
     "SerialDV is a minimal interface to encode and decode audio with AMBE3000
based devices in packet mode over a serial link.")
    (license license:gpl3+)))

(define-public cm256cc
  (package
    (name "cm256cc")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/f4exb/cm256cc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n9v7g6d370263bgqrjv38s9aq5953rzy7jvd8i30xq6aram9djg"))))
    (build-system cmake-build-system)
    (arguments
     ;; Disable some SIMD features for reproducibility.
     `(#:configure-flags '("-DENABLE_DISTRIBUTION=1")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./cm256_test")))))))
    (home-page "https://github.com/f4exb/cm256cc")
    (synopsis "Cauchy MDS Block Erasure Codec")
    (description
     "This is a C++ library implementing fast GF(256) Cauchy MDS Block Erasure
Codec.")
    (license license:gpl3+)))

(define-public libdab
  ;; No release since 2017, use commit directly.
  (let ((commit "b578d02eda60f613d35bab5d762ae7c9a27758d8")
        (revision "1"))
    (package
      (name "libdab")
      (version (git-version "0.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/JvanKatwijk/dab-cmdline")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0j339kx3n2plgfw7ikpp7b81h5n68wmsgflwljbh2sy8j62faik9"))))
      (build-system cmake-build-system)
      (inputs
       `(("faad2" ,faad2)
         ("fftwf" ,fftwf)
         ("zlib" ,zlib)))
      (arguments
       `(#:tests? #f  ; No test suite.
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-sources-directory
             (lambda _
               (chdir "library"))))))
      (home-page "https://github.com/JvanKatwijk/dab-cmdline")
      (synopsis "DAB decoding library")
      (description "This is a library to decode @acronym{DAB/DAB+, Digital
Audio Broadcasting}.")
      (license license:gpl2+))))

(define-public dsdcc
  (package
    (name "dsdcc")
    (version "1.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/f4exb/dsdcc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jgzpv4d6ckd0sdq6438rjh3m6knj6gx63627fajch74hxrvclzj"))))
    (build-system cmake-build-system)
    (inputs
     `(("mbelib" ,mbelib)
       ("serialdv" ,serialdv)))
    (arguments
     `(#:tests? #f  ; No test suite.
       #:configure-flags
       (list "-DUSE_MBELIB=ON"
             (string-append "-DLIBMBE_INCLUDE_DIR="
                            (assoc-ref %build-inputs "mbelib")
                            "/include")
             (string-append "-DLIBMBE_LIBRARY="
                            (assoc-ref %build-inputs "mbelib")
                            "/lib/libmbe.so")
             (string-append "-DLIBSERIALDV_INCLUDE_DIR="
                            (assoc-ref %build-inputs "serialdv")
                            "/include/serialdv")
             (string-append "-DLIBSERIALDV_LIBRARY="
                            (assoc-ref %build-inputs "serialdv")
                            "/lib/libserialdv.so"))))
    (home-page "https://github.com/f4exb/dsdcc")
    (synopsis "Digital speech decoder")
    (description
     "This package provides a library and a program to decode several digital
voice formats.")
    (license license:gpl3+)))

(define-public sdrangel
  (package
    (name "sdrangel")
    (version "6.16.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/f4exb/sdrangel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g9h4cy8k9dqlwkfk4lkk2d2s003bckzskm3vra87ndmgq1nfbzv"))))
    (build-system qt-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("airspyhf" ,airspyhf)
       ("alsa-lib" ,alsa-lib)
       ("aptdec" ,aptdec)
       ("boost" ,boost)
       ("cm256cc" ,cm256cc)
       ("codec2" ,codec2)
       ("dsdcc" ,dsdcc)
       ("faad2" ,faad2)
       ("ffmpeg" ,ffmpeg)
       ("fftwf" ,fftwf)
       ("hackrf" ,hackrf)
       ("libdab" ,libdab)
       ("libusb" ,libusb)
       ("mbelib" ,mbelib)
       ("opencv" ,opencv)
       ("opus" ,opus)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase-5)
       ("qtcharts" ,qtcharts)
       ("qtdeclarative" ,qtdeclarative)
       ("qtlocation" ,qtlocation)
       ("qtmultimedia" ,qtmultimedia)
       ("qtserialport" ,qtserialport)
       ("qtspeech" ,qtspeech)
       ("qtwebsockets" ,qtwebsockets)
       ("rtl-sdr" ,rtl-sdr)
       ("serialdv" ,serialdv)
       ("soapysdr" ,soapysdr)
       ("sgp4" ,sgp4)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f  ; No test suite.
       #:configure-flags
       (list (string-append "-DAPT_DIR="
                            (assoc-ref %build-inputs "aptdec"))
             (string-append "-DDAB_DIR="
                            (assoc-ref %build-inputs "libdab"))
             (string-append "-DDSDCC_DIR="
                            (assoc-ref %build-inputs "dsdcc"))
             (string-append "-DMBE_DIR="
                            (assoc-ref %build-inputs "mbelib"))
             (string-append "-DSERIALDV_DIR="
                            (assoc-ref %build-inputs "serialdv"))
             (string-append "-DSGP4_DIR="
                            (assoc-ref %build-inputs "sgp4"))
             (string-append "-DSOAPYSDR_DIR="
                            (assoc-ref %build-inputs "soapysdr")))))
    (home-page "https://github.com/f4exb/sdrangel/wiki")
    (synopsis "Software defined radio")
    (description
     "SDRangel is a Qt software defined radio and signal analyzer frontend for
various hardware.")
    (license license:gpl3+)))

(define-public sdr++
  (package
    (name "sdr++")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AlexandreRouma/SDRPlusPlus")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mplhys07l4bqv3q301ayh35468mg0hpxp5zgrps7gkjyf3v6idr"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gcc" ,gcc-10) ; A GCC more recent than version 7 is required.
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("airspyhf" ,airspyhf)
       ("alsa-lib" ,alsa-lib)
       ("fftwf" ,fftwf)
       ("glew" ,glew)
       ("glfw" ,glfw)
       ("hackrf" ,hackrf)
       ("jack" ,jack-2)
       ("libusb" ,libusb)
       ("pulseaudio" ,pulseaudio)
       ("rtaudio" ,rtaudio)
       ("rtl-sdr" ,rtl-sdr)
       ("soapysdr" ,soapysdr)
       ("volk" ,volk)))
    (arguments
     `(#:tests? #f ; No test suite.
       #:configure-flags '("-DOPT_BUILD_AIRSPY_SOURCE=OFF"
                           "-DOPT_BUILD_PLUTOSDR_SOURCE=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("/usr")
                (assoc-ref outputs "out"))))))))
    (home-page "https://github.com/AlexandreRouma/SDRPlusPlus")
    (synopsis "Software defined radio software")
    (description
     "SDR++ is a software defined radio software for various hardware.")
    (license license:gpl3+)))

(define-public inspectrum
  (package
    (name "inspectrum")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/miek/inspectrum")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x6nyn429pk0f7lqzskrgsbq09mq5787xd4piic95add6n1cc355"))))
    (build-system qt-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fftwf" ,fftwf)
       ("liquid-dsp" ,liquid-dsp)
       ("qtbase" ,qtbase-5)))
    (home-page "https://github.com/miek/inspectrum")
    (synopsis "Radio signal analyser")
    (description
     "Inspectrum is a tool for analysing captured signals, primarily from
software-defined radio receivers.")
    (license license:gpl3+)))

(define-public wfview
  (package
    (name "wfview")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/eliggett/wfview")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16a9afm0nkqx4pzwfxisspybimhqdyr3yjpr7ac7wgpp3520ikzi"))))
    (build-system qt-build-system)
    (inputs
     `(("qcustomplot" ,qcustomplot)
       ("qtbase" ,qtbase-5)
       ("qtmultimedia" ,qtmultimedia)
       ("qtserialport" ,qtserialport)))
    (arguments
     `(#:tests? #f  ; No test suite.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "wfview.pro"
               (("\\.\\./wfview/")
                "../"))
             (substitute* '("wfmain.cpp")
               (("/usr/share")
                (string-append (assoc-ref outputs "out") "/share")))))
         (replace 'configure
           (lambda _
             (mkdir-p "build")
             (chdir "build")
             (invoke "qmake" "../wfview.pro")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (install-file "wfview"
                             (string-append out "/bin"))
               (install-file "wfview.png"
                             (string-append out "/share/pixmaps"))
               (install-file "wfview.desktop"
                             (string-append out "/share/applications"))
               (let ((dir (string-append
                           out "/share/wfview/stylesheets/qdarkstyle")))
                 (mkdir-p dir)
                 (copy-recursively "qdarkstyle" dir))))))))
    (home-page "https://wfview.org/")
    (synopsis "Software to control Icom radios")
    (description
     "@code{wfview} is a program to control modern Icom radios and view the
spectrum waterfall.  It supports at least the following models:

@itemize
@item IC-705
@item IC-7300
@item IC-7610
@item IC-7850
@item IC-7851
@item IC-9700
@end itemize\n")
    (license (list license:expat
                   license:gpl3))))

(define-public minimodem
  (package
    (name "minimodem")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.whence.com/minimodem/minimodem-"
                           version ".tar.gz"))
       (sha256
        (base32 "13ipyh39l7p420j1j9kvwyskv2nqnimls1a3z1klsa1zivds9k7q"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fftwf" ,fftwf)
       ("libsndfile" ,libsndfile)
       ("pulseaudio" ,pulseaudio)))
    (home-page "http://www.whence.com/minimodem/")
    (synopsis "Software audio FSK modem")
    (description
     "Minimodem is a command-line program which decodes (or generates) audio
modem tones at any specified baud rate, using various framing protocols.  It
acts a general-purpose software FSK modem, and includes support for various
standard FSK protocols such as Bell103, Bell202, RTTY, TTY/TDD, NOAA SAME, and
Caller-ID.")
    (license license:gpl3+)))

(define-public rfcat
  ;; Use a commit for now because some fixes to make rfcat work with
  ;; Python 3 instead of Python 2 are not in a release yet.
  (let ((commit "725bf79af27d47cdec64107317c1c8fe3f7ad7b8")
        (revision "1"))
    (package
      (name "rfcat")
      (version (git-version "1.9.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/atlas0fd00m/rfcat")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0dbc6n4pxsa73wzxny773khc73r1dn3ma5hi7xv76vcykjvzkdi3"))))
      (build-system python-build-system)
      (inputs
       `(("python-future" ,python-future)
         ("python-ipython" ,python-ipython)
         ("python-numpy" ,python-numpy)
         ("python-pyserial" ,python-pyserial)
         ("python-pyside-2" ,python-pyside-2)
         ("python-pyusb" ,python-pyusb)))
      (arguments
       `(#:tests? #f  ; Tests want to use a serial port
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-permissions
             (lambda _
               (make-file-writable "rflib/rflib_version.py")))
           (add-after 'install 'install-udev-rules
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "etc/udev/rules.d/20-rfcat.rules"
                             (string-append (assoc-ref outputs "out")
                                            "/lib/udev/rules.d")))))))
      (home-page "https://github.com/atlas0fd00m/rfcat")
      (synopsis "Program to control some radio dongles")
      (description
       "@code{rfcat} is a program to control some radio dongles operating in
ISM bands.

Supported dongles:
@itemize
@item YARD Stick One
@item cc1111emk
@item chronos watch dongle
@item imme (limited support)
@end itemize

To install the rfcat udev rules, you must extend @code{udev-service-type} with
this package.  E.g.: @code{(udev-rules-service 'rfcat rfcat)}")
      (license (list license:bsd-3
                     license:gpl2)))))

(define-public rx-tools
  ;; No tagged release since 2016, use commit instead.
  (let ((commit "811b21c4c8a592515279bd19f7460c6e4ff0551c")
        (revision "1"))
    (package
      (name "rx-tools")
      (version (git-version "1.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rxseger/rx_tools")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qr8q00cv6q0ikjrph0qh07mlbvgk4yimccpkn3ir8ib5ma0r9sr"))))
      (build-system cmake-build-system)
      (inputs
       `(("soapysdr" ,soapysdr)))
      (arguments
       `(#:tests? #f)) ; No test suite.
      (home-page "https://github.com/rxseger/rx_tools")
      (synopsis "Command line programs for receiving data from SDRs")
      (description
       "This package provides the @code{rx_fm}, @code{rx_power} and
@code{rx_sdr} tools for receiving data from SDRs, based on @code{rtl_fm},
@code{rtl_power} and @code{rtl_sdr} from RTL-SDR, but using the SoapySDR
vendor-neutral SDR support library instead, intended to support a wider range
of devices than RTL-SDR.")
      (license license:gpl2+))))

(define-public gnss-sdr
  (package
    (name "gnss-sdr")
    (version "0.0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnss-sdr/gnss-sdr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kjh9bnf6h9q71bnn8nrwlc80wcnkib97ylzvb102acii4p0fm08"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)
       ("googletest-source" ,(package-source googletest))
       ("orc" ,orc)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python-mako" ,python-mako)))
    (inputs
     `(("armadillo" ,armadillo)
       ("boost" ,boost)
       ("gflags" ,gflags)
       ("glog" ,glog)
       ("gmp" ,gmp)
       ("gnuradio" ,gnuradio)
       ("gr-osmosdr" ,gr-osmosdr)
       ("lapack" ,lapack)
       ("libpcap" ,libpcap)
       ("log4cpp" ,log4cpp)
       ("matio" ,matio)
       ("openblas" ,openblas)
       ("openssl" ,openssl)
       ("protobuf" ,protobuf)
       ("pugixml" ,pugixml)
       ("volk" ,volk)))
    (arguments
     `(#:configure-flags
       (list "-DENABLE_GENERIC_ARCH=ON"
             "-DENABLE_OSMOSDR=ON"
             (string-append "-DGFLAGS_ROOT="
                            (assoc-ref %build-inputs "gflags"))
             (string-append "-DGLOG_ROOT="
                            (assoc-ref %build-inputs "glog"))
             (string-append "-DGTEST_DIR="
                            (assoc-ref %build-inputs "googletest-source")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; Some tests fail to compile when the FILESYSTEM package is
             ;; available, so we disable it (and the tests will use Boost
             ;; Filesystem instead).
             (substitute* "CMakeLists.txt"
               (("find_package\\(FILESYSTEM COMPONENTS Final Experimental\\)")
                ""))))
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (home-page "https://gnss-sdr.org/")
    (synopsis "Global Navigation Satellite Systems software-defined receiver")
    (description
     "This program is a software-defined receiver which is able to process
(that is, to perform detection, synchronization, demodulation and decoding of
the navigation message, computation of observables and, finally, computation of
position fixes) the signals of the BeiDou, Galileo, GLONASS and GPS Global
Navigation Satellite System.")
    (license license:gpl3+)))
