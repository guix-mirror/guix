;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019, 2020 Christopher Howard <christopher@librehacker.com>
;;; Copyright © 2019, 2020 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2020, 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
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
  #:use-module (guix gexp)
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
     (list autoconf automake))
    (inputs
     (list fftwf libfec))
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
     (list libusb))
    (native-inputs
     (list pkg-config))
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
     (list pkg-config))
    (inputs
     (list libusb))
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
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pothosware/SoapySDR")
             (commit (string-append "soapy-sdr-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19f2x0pkxvf9figa0pl6xqlcz8fblvqb19mcnj632p0l8vk6qdv2"))))
    (build-system cmake-build-system)
    (native-inputs
     (list python swig))
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
     (list airspyhf soapysdr))
    (arguments
     `(#:tests? #f))  ; No test suite
    (home-page "https://github.com/pothosware/SoapyAirspyHF/wiki")
    (synopsis "SoapySDR Airspy HF+ module")
    (description "This package provides Airspy HF+ devices support to the
SoapySDR library.")
    (license license:expat)))

(define-public soapyhackrf
  (package
    (name "soapyhackrf")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pothosware/SoapyHackRF")
             (commit (string-append "soapy-hackrf-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wnnsm3c4ivwcbs1b68svbyds13aajws9mqk2xjc2pqgj0fdhcvz"))))
    (build-system cmake-build-system)
    (inputs
     (list hackrf soapysdr))
    (arguments
     `(#:tests? #f))  ; No test suite
    (home-page "https://github.com/pothosware/SoapyHackRF/wiki")
    (synopsis "SoapySDR HackRF module")
    (description
     "This package provides HackRF devices support to the SoapySDR library.")
    (license license:expat)))

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
     (list rtl-sdr soapysdr))
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
    (version "20220118")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://trac.chirp.danplanet.com/chirp_daily/daily-"
                           version "/chirp-daily-" version ".tar.gz"))
       (sha256
        (base32 "0q2ifmprxqqxnww8fx8fjmm2lv9vdv6xzsk1ysa27fb290vgyawn"))))
    (build-system python-build-system)
    (inputs
     (list python2-libxml2 python2-pygtk python2-pyserial))
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
  (let ((commit "51405971fd4e97714d1e987269e49c6edfe4e0da")
        (revision "2"))
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
          (base32 "0fzkhqnbkc2dd39jzb5m5mwcl31b0x3w0yykpx4s195llq4bdvis"))))
      (build-system cmake-build-system)
      (inputs
       (list libpng libsndfile))
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
    (version "0.20")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/windytan/redsea")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bx4l87vz935cj5hapdh1dkjlmlfg73cgsjaf27314n7p4xkv50v"))))
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
     (list libiconv libsndfile liquid-dsp))
    (native-inputs
     (list autoconf automake))
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
       ("texlive" ,(texlive-updmap.cfg (list texlive-amsfonts
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
       ("python-pyqtgraph" ,python-pyqtgraph)
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
             (system "Xvfb :1 &")
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
                             "qa_packet_headerparser_b"
                             ;; qa_rotator_cc sometimes fails, it looks like
                             ;; a floating point number precision issue.
                             "qa_rotator_cc")
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
            (variable "GUIX_PYTHONPATH")
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
       (list doxygen pkg-config pybind11 python-mako python-six))
      (inputs
       (list airspyhf
             boost
             fftwf
             gmp
             gnuradio
             hackrf
             libsndfile
             log4cpp
             python
             python-numpy
             python-pyqt
             rtl-sdr
             soapysdr
             volk))
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
       ("texlive" ,(texlive-updmap.cfg (list texlive-amsfonts
                                        texlive-latex-amsmath
                                        ;; TODO: Add newunicodechar.
                                        texlive-latex-graphics)))))
    (inputs
     (list fftwf))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "git-version-gen"
               (("/bin/sh")
                (search-input-file inputs "/bin/bash"))))))))
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
       (list doxygen
             pkg-config
             pybind11
             python
             python-numpy
             python-six))
      (inputs
       (list boost
             fftwf
             gmp
             gnuradio
             libosmo-dsp
             log4cpp
             volk))
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
     (list pkg-config pybind11 python-six))
    (inputs
     (list boost
           gmp
           gnuradio
           log4cpp
           python
           python-construct
           python-numpy
           python-pyaml
           python-pyzmq
           python-requests
           volk))
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
    (version "2.15.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gqrx-sdr/gqrx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ph7fnjxfv7rfdlh17c8j4djvkfqjnalmcvnafhnq7agjy428727"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           boost
           fftwf
           gmp
           gnuradio
           gr-iqbal
           gr-osmosdr
           jack-1
           libsndfile
           log4cpp
           portaudio
           pulseaudio
           qtbase-5
           qtsvg
           volk))
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
     (list autoconf automake gettext-minimal pkg-config))
    (inputs
     (list alsa-lib
           fltk
           eudev
           hamlib
           libpng
           libsamplerate
           libusb
           libx11
           libxext
           libxfixes
           libxft
           portaudio
           pulseaudio))
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
    (version "1.4.04")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/flrig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06alzln46x08110v1ghasphr2mmznzk0x5h59vl9g2w1z12i9zsm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list eudev fltk libx11 libxext libxfixes libxft))
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
    (version "2.2.07")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/flamp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rygd5w04nspxdj8qj81gpb3mgijvlmii74s1f4mihqs5kb8nwh6"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list fltk libx11 libxext libxfixes libxft))
    (synopsis "Tool for AMP file transfer")
    (description
     "FLAMP is a program for transferring files by radio waves using AMP
(Amateur Multicast Protocol).")
    (home-page "http://www.w1hkj.com/")
    (license license:gpl3+)))

(define-public flmsg
  (package
    (name "flmsg")
    (version "4.0.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/fldigi/flmsg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "157s6mvky9h094hkncm47964qh0bnwg0m2rw3wx0qj1mh5isxv4j"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list fltk libx11 libxext libxfixes libxft))
    (synopsis "NBEMS messaging system")
    (description
     "FLMSG is a Narrow Band Emergency Messaging Software (NBEMS).
It can be used to manage, send and receive the forms that are used as basis
for emergency communications data transfers (like ICS213 forms).")
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
     (list autoconf automake pkg-config))
    (inputs
     (list fltk libx11 libxext libxfixes libxft))
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
     (list pkg-config))
    (inputs
     (list fftw fftwf libusb))
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
    (version "4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/Hamlib/Hamlib/releases/download/"
             version "/hamlib-" version ".tar.gz"))
       (sha256
        (base32 "11r4i8gmxnb6ixpk4ns38c9xwj3qibp2v3pkhy2z0lhz0xxi1w4b"))))
    (build-system gnu-build-system)
    (native-inputs
     (list doxygen
           lua
           pkg-config
           python-wrapper
           swig
           tcl))
    (inputs
     (list gd libusb libxml2 readline))
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
    (version "2.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/u/bsomervi/hamlib.git")
             (commit (string-append "wsjtx-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bgf7bz2280739a7ip7lvpns0i7x6svryxfmsp32cff2dr146lz3"))))
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
     (list autoconf automake perl pkg-config))
    (inputs
     (list cmocka
           glib
           hamlib
           libusb ;`Requires.private: libusb-1.0` in hamlib pkg-config
           ncurses
           xmlrpc-c))
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
    (version "2.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/wsjt/wsjtx.git")
             (commit (string-append "wsjtx-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nciw9smrfcsirlwyny5r9h7sk2zvm40m56y1hxpgpmbnh6mqikh"))))
    (build-system qt-build-system)
    (native-inputs
     (list asciidoc gfortran pkg-config qttools ruby-asciidoctor))
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
     `(#:tests? #f)) ; No test suite
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
     (list asciidoc gfortran pkg-config qttools ruby-asciidoctor))
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
                               "/share")))))
         (add-after 'unpack 'fix-hamlib
           (lambda _
             (substitute* "CMake/Modules/Findhamlib.cmake"
               (("set \\(ENV\\{PKG_CONFIG_PATH\\}.*\\)")
                "set (__pc_path $ENV{PKG_CONFIG_PATH})
  list (APPEND __pc_path \"${__hamlib_pc_path}\")
  set (ENV{PKG_CONFIG_PATH} \"${__pc_path}\")"))
             (substitute* "HamlibTransceiver.hpp"
               (("#ifdef JS8_USE_LEGACY_HAMLIB")
                "#if 1")))))))
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
    (version "4.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.xnec2c.org/releases/xnec2c-v"
                           version ".tar.gz"))
       (sha256
        (base32 "0v3qr16d42jri2vwwgrhhknaypdcjyn6ccdjpqfzr8zzr33z5pca"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           `(,glib "bin")
           libtool
           pkg-config))
    (inputs
     (list gtk+ openblas))
    (arguments
     `(#:configure-flags
       ,#~(list (string-append "--with-openblas-incdir="
                               #$(this-package-input "openblas")
                               "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/Makefile.am"
               (("\\$\\(GLIB_COMPILE_RESOURCES\\)")
                (search-input-file inputs "bin/glib-compile-resources")))
             (substitute* "src/mathlib.c"
               (("libopenblas\\.so")
                (search-input-file inputs "lib/libopenblas.so"))))))))
    (synopsis "Antenna modeling software")
    (description
     "Xnec2c is a GTK3-based graphical version of nec2c, a translation to the
C language of NEC2, the FORTRAN Numerical Electromagnetics Code commonly used
for antenna simulation and analysis.  It can be used to define the geometry of
an antenna, and then plot the radiation pattern or frequency-related data like
gain and standing wave ratio.")
    (home-page "https://www.xnec2c.org/")
    (license license:gpl3+)))

(define-public dump1090
  (package
    (name "dump1090")
    (version "7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flightaware/dump1090")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1702qv5g27sgpqf98sj2qbg36sqzc7r3kssslzzgljqkr5jgrw6m"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list hackrf libusb ncurses rtl-sdr))
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
               (install-file "view1090" bin)))))))
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
    (version "21.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/merbanan/rtl_433")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ygxs35zvgnamhqdwk1akcwagcirzpi4wndzgf9d23faiv4cm01a"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libusb openssl rtl-sdr soapysdr))
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
     (list libx11 pulseaudio))
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
     (list python-cython))
    (inputs
     (list python-numpy python-pyqt python-pyserial python-scipy))
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
    (version "9.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://users.telenet.be/on4qz/qsstv/downloads/"
                           "qsstv_" version ".tar.gz"))
       (sha256
        (base32 "0s3sivc0xan6amibdiwfnknrl3248wzgy98w6gyxikl0qsjpygy0"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list alsa-lib
           fftw
           fftwf
           hamlib
           openjpeg
           pulseaudio
           qtbase-5
           v4l-utils))
    (arguments
     `(#:tests? #f  ; No test suite.
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))))))))
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
     (list alsa-lib hamlib))
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
     (list ao))
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
     (list pkg-config))
    (inputs
     (list alsa-lib ncurses pulseaudio qtbase-5))
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
                (search-input-file inputs "/lib/libpulse-simple.so"))))))))
    (home-page "http://unixcw.sourceforge.net/")
    (synopsis "Morse code library and programs")
    (description
     "@code{unixcw} is a project providing the libcw library and a set of
programs using the library: cw, cwgen, cwcp and xcwcp.  The programs are
intended for people who want to learn receiving and sending morse code.")
    (license license:gpl2+)))

(define-public kochmorse
  (package
    (name "kochmorse")
    (version "3.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hmatuschek/kochmorse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s1aj223n57rpc95rih98z08xnyhq2zp02byzrc3f7s01fv3nj0l"))))
    (build-system qt-build-system)
    (native-inputs
     (list qttools))
    (inputs
     (list qtbase-5 qtmultimedia))
    (arguments
     `(#:tests? #f)) ; No test suite
    (home-page "https://dm3mat.darc.de/kochmorse/")
    (synopsis "Morse code tutor")
    (description
     "KochMorse is a simple morse-code tutor using the Koch method.")
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
     `(#:configure-flags '("-DCMAKE_C_FLAGS=-fcommon")
       #:tests? #f ; No test suite
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
     (list go-github-com-akosmarton-papipes
           go-github-com-fatih-color
           go-github-com-google-goterm
           go-github-com-mattn-go-isatty
           go-github-com-mesilliac-pulse-simple
           go-github-com-pborman-getopt
           go-go-uber-org-multierr
           go-go-uber-org-zap))
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
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/drm/dream/" version
                           "/dream_" version ".orig.tar.gz"))
       (sha256
        (base32 "0mpg341b0vnm6ym0cag9zri9w6kw012rv68zdmmi2hlvq7iiw8gp"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list faad2
           fftw
           libsndfile
           libpcap
           opus
           pulseaudio
           qtbase-5
           qtsvg
           qtwebkit
           qwt
           speexdsp
           zlib))
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
               (("/usr/include/pulse")
                (search-input-directory inputs "/include/pulse"))
               (("/usr/include/sndfile\\.h")
                (search-input-file inputs "/include/sndfile.h"))
               (("/usr/include/opus")
                (search-input-directory inputs "/include/opus"))
               (("/usr/include/speex")
                (search-input-directory inputs "/include/speex"))
               (("/usr/include/qwt")
                (search-input-directory inputs "/include/qwt"))
               (("\\$\\$OUT_PWD/include/neaacdec\\.h")
                (search-input-file inputs "/include/neaacdec.h")))))
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
     (list pkg-config))
    (inputs
     (list alsa-lib
           faad2
           fftwf
           lame
           libusb
           mpg123
           rtl-sdr
           qtbase-5
           qtcharts
           qtdeclarative
           qtgraphicaleffects
           qtmultimedia
           qtquickcontrols2
           soapysdr))
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
       (list fftwf))
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
       (list faad2 fftwf zlib))
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
     (list mbelib serialdv))
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
    (version "6.18.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/f4exb/sdrangel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17lakqy55p481fa5w6ylh79xbd4rdiqk9y21vmi4m2r4wx893zw1"))))
    (build-system qt-build-system)
    (native-inputs
     (list doxygen graphviz pkg-config))
    (inputs
     (list airspyhf
           alsa-lib
           aptdec
           boost
           cm256cc
           codec2
           dsdcc
           faad2
           ffmpeg
           fftwf
           hackrf
           libdab
           libusb
           mbelib
           opencv
           opus
           pulseaudio
           qtbase-5
           qtcharts
           qtdeclarative
           qtlocation
           qtmultimedia
           qtquickcontrols2
           qtserialport
           qtspeech
           qtwebsockets
           rtl-sdr
           serialdv
           soapysdr
           sgp4
           zlib))
    (arguments
     `(#:tests? #f  ; No test suite.
       #:configure-flags
       ,#~(list (string-append "-DAPT_DIR="
                               #$(this-package-input "aptdec"))
                (string-append "-DDAB_DIR="
                               #$(this-package-input "libdab"))
                (string-append "-DDSDCC_DIR="
                               #$(this-package-input "dsdcc"))
                (string-append "-DMBE_DIR="
                               #$(this-package-input "mbelib"))
                (string-append "-DSERIALDV_DIR="
                               #$(this-package-input "serialdv"))
                (string-append "-DSGP4_DIR="
                               #$(this-package-input "sgp4"))
                (string-append "-DSOAPYSDR_DIR="
                               #$(this-package-input "soapysdr")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-boost-compatibility
           (lambda _
             (substitute*
                 '("plugins/channelrx/noisefigure/noisefigure.cpp"
                   "plugins/channelrx/noisefigure/noisefigureenrdialog.cpp")
               (("boost::math::barycentric_rational<double>")
                "boost::math::interpolators::barycentric_rational<double>")))))))
    (home-page "https://github.com/f4exb/sdrangel/wiki")
    (synopsis "Software defined radio")
    (description
     "SDRangel is a Qt software defined radio and signal analyzer frontend for
various hardware.")
    (license license:gpl3+)))

(define-public sdr++
  (package
    (name "sdr++")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AlexandreRouma/SDRPlusPlus")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xwbz6yyca6wmzad5ykxw6i0r8jzc7i3jbzq7mhp8caiymd6knw3"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     `(("airspyhf" ,airspyhf)
       ("alsa-lib" ,alsa-lib)
       ("codec2" ,codec2)
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
                           "-DOPT_BUILD_PLUTOSDR_SOURCE=OFF"
                           "-DOPT_BUILD_M17_DECODER=ON")
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
     (list pkg-config))
    (inputs
     (list fftwf liquid-dsp qtbase-5))
    (home-page "https://github.com/miek/inspectrum")
    (synopsis "Radio signal analyser")
    (description
     "Inspectrum is a tool for analysing captured signals, primarily from
software-defined radio receivers.")
    (license license:gpl3+)))

(define-public wfview
  (package
    (name "wfview")
    (version "1.2d")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/eliggett/wfview")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kpkwxhcacgmprbr8xz840rj9a22513vxrh2q7d3js5i1dva8j2z"))))
    (build-system qt-build-system)
    (inputs
     (list opus qcustomplot qtbase-5 qtmultimedia qtserialport))
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
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p "build")
             (chdir "build")
             (invoke "qmake"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))
                     "../wfview.pro"))))))
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
     (list pkg-config))
    (inputs
     (list alsa-lib fftwf libsndfile pulseaudio))
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
       (list python-future
             python-ipython
             python-numpy
             python-pyserial
             python-pyside-2
             python-pyusb))
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
       (list soapysdr))
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
    (version "0.0.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gnss-sdr/gnss-sdr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m41rnlfr1nrzbg382jfsk5x0by2ym48v3innd2rbc6phd85q223"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)
       ("googletest-source" ,(package-source googletest))
       ("orc" ,orc)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python-mako" ,python-mako)))
    (inputs
     (list armadillo
           boost
           gflags
           glog
           gmp
           gnuradio
           gr-osmosdr
           lapack
           libpcap
           log4cpp
           matio
           openblas
           openssl
           protobuf
           pugixml
           volk))
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
