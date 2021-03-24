;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 by Amar Singh <nly@disroot.org>
;;; Copyright © 2020 R Veera Kumar <vkor@vkten.in>
;;; Copyright © 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages astronomy)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public cfitsio
  (package
    (name "cfitsio")
    (version "3.49")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/"
             "cfitsio-" version ".tar.gz"))
       (sha256
        (base32 "1cyl1qksnkl3cq1fzl4dmjvkd6329b57y9iqyv44wjakbh6s4rav"))))
    (build-system gnu-build-system)
    ;; XXX Building with curl currently breaks wcslib.  It doesn't use
    ;; pkg-config and hence won't link with -lcurl.
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda _
             (substitute* "Makefile.in" (("/bin/") ""))
             #t)))))
    (home-page "https://heasarc.gsfc.nasa.gov/fitsio/fitsio.html")
    (synopsis "Library for reading and writing FITS files")
    (description "CFITSIO provides simple high-level routines for reading and
writing @dfn{FITS} (Flexible Image Transport System) files that insulate the
programmer from the internal complexities of the FITS format. CFITSIO also
provides many advanced features for manipulating and filtering the information
in FITS files.")
    (license (license:non-copyleft "file://License.txt"
                          "See License.txt in the distribution."))))

(define-public qfits
  (package
    (name "qfits")
    (version "6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "ftp://ftp.eso.org/pub/qfits/qfits-" version ".tar.gz"))
       (sha256
        (base32 "0m2b21mim3a7wgfg3ph2w5hv7mdvr03jmmhzipc0wcahijglcw9j"))))
    (build-system gnu-build-system)
    (home-page "https://www.eso.org/sci/software/eclipse/qfits/")
    (synopsis "C library offering access to astronomical FITS files")
    (description
     "@code{qfits} is a C library giving access to FITS file internals, both
for reading and writing.")
    (license license:gpl2+)))

(define-public erfa
  (package
    (name "erfa")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/liberfa/erfa")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vsqwvzlk7r8q7nwyb7i710blcfdl5kwcm2va9km07a820nsp84a"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("automake" ,automake)
       ("autoreconf" ,autoconf)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/liberfa/erfa")
    (synopsis "Essential Routines for Fundamental Astronomy")
    (description
     "ERFA is a C library containing key algorithms for astronomy, and is based
on the SOFA library published by the International Astronomical Union (IAU).")
    (license license:bsd-3)))

(define-public eye
  (package
    (name "eye")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/eye/"
                           "eye-" version ".tar.gz"))
       (sha256
        (base32 "092qhzcbrkcfidbx4bv9wz42w297n80jk7a6kwyi9a3fjfz81d7k"))))
    (build-system gnu-build-system)
    (home-page "https://www.astromatic.net/software/eye")
    (synopsis "Small image feature detector using machine learning")
    (description
     "In EyE (Enhance Your Extraction) an artificial neural network connected to
pixels of a moving window (retina) is trained to associate these input stimuli
to the corresponding response in one or several output image(s).  The resulting
filter can be loaded in SExtractor to operate complex, wildly non-linear filters
on astronomical images.  Typical applications of EyE include adaptive filtering,
feature detection and cosmetic corrections.")
    (license license:cecill)))

(define-public wcslib
  (package
    (name "wcslib")
    (version "7.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.atnf.csiro.au/pub/software/wcslib/wcslib-" version
             ".tar.bz2"))
       (sha256
        (base32 "1536gmcpm6pckn9xrb6j8s4pm1vryjhzvhfaj9wx3jwxcpbdy0dw"))))
    (inputs
     `(("cfitsio" ,cfitsio)))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-cfitsiolib="
                            (assoc-ref %build-inputs "cfitsio") "/lib")
             (string-append "--with-cfitsioinc="
                            (assoc-ref %build-inputs "cfitsio") "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-/bin/sh
           (lambda _
             (substitute* "makedefs.in"
               (("/bin/sh") "sh"))
             #t))
         (delete 'install-license-files)) ; installed by ‘make install’
       ;; Parallel execution of the test suite is not supported.
       #:parallel-tests? #f))
    (home-page "https://www.atnf.csiro.au/people/mcalabre/WCS")
    (synopsis "Library which implements the FITS WCS standard")
    (description "The FITS \"World Coordinate System\" (@dfn{WCS}) standard
defines keywords and usage that provide for the description of astronomical
coordinate systems in a @dfn{FITS} (Flexible Image Transport System) image
header.")
    (license license:lgpl3+)))

(define-public weightwatcher
  (package
    (name "weightwatcher")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/weightwatcher/"
                           "weightwatcher-" version ".tar.gz"))
       (sha256
        (base32 "1zaqd8d9rpgcwjsp92q3lkfaa22i20gppb91dz34ym54swisjc2p"))))
    (build-system gnu-build-system)
    (home-page "https://www.astromatic.net/software/weightwatcher")
    (synopsis "Weight-map/flag-map multiplexer and rasteriser")
    (description
     "Weightwatcher is a program hat combines weight-maps, flag-maps and
polygon data in order to produce control maps which can directly be used in
astronomical image-processing packages like Drizzle, Swarp or SExtractor.")
    (license license:gpl3+)))

(define-public gnuastro
  (package
    (name "gnuastro")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnuastro/gnuastro-"
                           version ".tar.lz"))
       (sha256
        (base32
         "1xp6n42qxv0x6yigi2w2l5k8006smv27lhrcssysgsvzbydghzg5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (inputs
     `(("cfitsio" ,cfitsio)
       ("curl" ,curl-minimal)
       ("gsl" ,gsl)
       ("libgit2" ,libgit2)
       ("libjpeg" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("wcslib" ,wcslib)
       ("zlib" ,zlib)))
    (native-inputs
     `(("libtool" ,libtool)
       ("lzip" ,lzip)))
    (home-page "https://www.gnu.org/software/gnuastro/")
    (synopsis "Astronomy utilities")
    (description "The GNU Astronomy Utilities (Gnuastro) is a suite of
programs for the manipulation and analysis of astronomical data.")
    (license license:gpl3+)))

(define-public sextractor
  (package
    (name "sextractor")
    (version "2.25.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/sextractor")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q69n3nyal57h3ik2xirwzrxzljrwy9ivwraxzv9566vi3n4z5mw"))))
    (build-system gnu-build-system)
    ;; NOTE: (Sharlatan-20210124T103117+0000): Building with `atlas' is failing
    ;; due to missing shared library which required on configure phase. Switch
    ;; build to use `openblas' instead. It requires FFTW with single precision
    ;; `fftwf'.
    (arguments
     `(#:configure-flags
       (list
        "--enable-openblas"
        (string-append
         "--with-openblas-libdir=" (assoc-ref %build-inputs "openblas") "/lib")
        (string-append
         "--with-openblas-incdir=" (assoc-ref %build-inputs "openblas") "/include")
        (string-append
         "--with-fftw-libdir=" (assoc-ref %build-inputs "fftw") "/lib")
        (string-append
         "--with-fftw-incdir=" (assoc-ref %build-inputs "fftw") "/include"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("openblas" ,openblas)
       ("fftw" ,fftwf)))
    (home-page "http://www.astromatic.net/software/sextractor")
    (synopsis "Extract catalogs of sources from astronomical images")
    (description
     "SExtractor is a program that builds a catalogue of objects from an
astronomical image.  Although it is particularly oriented towards reduction of
large scale galaxy-survey data, it can perform reasonably well on moderately
crowded star fields.")
    (license license:gpl3+)))

(define-public skymaker
  (package
    (name "skymaker")
    (version "3.10.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/skymaker/"
                           "skymaker-" version ".tar.gz"))
       (sha256
        (base32 "03zvx7c89plp9559niqv5532r233kza3ir992rg3nxjksqmrqvx1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        (string-append
         "--with-fftw-libdir=" (assoc-ref %build-inputs "fftw") "/lib")
        (string-append
         "--with-fftw-incdir=" (assoc-ref %build-inputs "fftw") "/include"))))
    (inputs
     `(("fftw" ,fftwf)))
    (home-page "https://www.astromatic.net/software/skymaker")
    (synopsis "Astronomical image simulator")
    (description
     "SkyMaker is a program that simulates astronomical images.  It accepts
object lists in ASCII generated by the Stuff program to produce realistic
astronomical fields.  SkyMaker is part of the EFIGI
(@url{https://www.astromatic.net/projects/efigi}) development project.")
    (license license:gpl3+)))

(define-public stackistry
  (package
    (name "stackistry")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GreatAttractor/stackistry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rz29v33n0x0k40hv3v79ym5ylch1v0pbph4i21809gz2al5p7dq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list
        (string-append
         "SKRY_INCLUDE_PATH=" (assoc-ref %build-inputs "libskry") "/include")
        (string-append
         "SKRY_LIB_PATH=-L" (assoc-ref %build-inputs "libskry") "/lib")
        (string-append
         "LIBAV_INCLUDE_PATH=" (assoc-ref %build-inputs "ffmpeg") "/include"))
       #:phases
       (modify-phases %standard-phases
         ;; no configure and tests are provided
         (delete 'configure)
         (delete 'check)
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/main.cpp"
               (("\"\\.\\.\", \"lang\"")
                "\"../share/stackistry\", \"lang\""))
             (substitute* "src/utils.cpp"
               (("\"\\.\\.\", \"icons\"")
                "\"../share/stackistry\", \"icons\""))
             #t))
         (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (icons (string-append out "/share/stackistry/icons"))
                    (lang (string-append out "/share/stackistry/lang")))
               (copy-recursively "bin" bin)
               (copy-recursively "icons" icons)
               (copy-recursively "lang" lang))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
     (inputs
      `(("gtkmm" ,gtkmm)
        ("libskry" ,libskry)
        ("ffmpeg" ,ffmpeg)))
     (home-page "https://github.com/GreatAttractor/stackistry")
     (synopsis "Astronomical lucky imaging/image stacking tool")
     (description
      "Stackistry implements the lucky imaging principle of astronomical
imaging: creating a high-quality still image out of a series of many (possibly
thousands) low quality ones (blurred, deformed, noisy).  The resulting image
stack typically requires post-processing, including sharpening (e.g. via
deconvolution).  Such post-processing is not performed by Stackistry.")
     (license license:gpl3+)))

(define-public stellarium
  (package
    (name "stellarium")
    (version "0.20.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Stellarium/stellarium"
                           "/releases/download/v" version
                           "/stellarium-" version ".tar.gz"))
       (sha256
        (base32 "1253zlr0mi4kdbj119spxk7spg4rkahb4rlpd0hz1d81mnv3n0v3"))))
    (build-system cmake-build-system)
    (inputs
     `(("qtbase" ,qtbase)
       ("qtlocation" ,qtlocation)
       ("qtmultimedia" ,qtmultimedia)
       ("qtscript" ,qtscript)
       ("qtserialport" ,qtserialport)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gettext" ,gettext-minimal)     ; xgettext is used at compile time
       ("perl" ,perl)                   ; for pod2man
       ("qtbase" ,qtbase)               ; Qt MOC is needed at compile time
       ("qttools" ,qttools)))
    (arguments
     `(#:test-target "test"
       #:configure-flags (list "-DENABLE_TESTING=1"
                               (string-append
                                "-DCMAKE_CXX_FLAGS=-isystem "
                                (assoc-ref %build-inputs "qtserialport")
                                "/include/qt5"))
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'set-offscreen-display
                    (lambda _
                      ;; Make Qt render "offscreen", required for tests.
                      (setenv "QT_QPA_PLATFORM" "offscreen")
                      (setenv "HOME" "/tmp")
                      #t)))))
    (home-page "https://stellarium.org/")
    (synopsis "3D sky viewer")
    (description "Stellarium is a planetarium.  It shows a realistic sky in
3D, just like what you see with the naked eye, binoculars, or a telescope.  It
can be used to control telescopes over a serial port for tracking celestial
objects.")
    (license license:gpl2+)))

(define-public stuff
  (package
    (name "stuff")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/stuff/"
                           "stuff-" version ".tar.gz"))
       (sha256
        (base32 "1syibi3b86z9pikhicvkkmgxm916j732fdiw0agw0lq6z13fdcjm"))))
    (build-system gnu-build-system)
    (home-page "https://www.astromatic.net/software/stuff")
    (synopsis "Astronomical catalogue simulation")
    (description
     "Stuff is a program that simulates \"perfect\" astronomical catalogues.
It generates object lists in ASCII which can read by the SkyMaker program to
produce realistic astronomical fields.  Stuff is part of the EFIGI development
project.")
    (license license:gpl3+)))

(define-public swarp
  (package
    (name "swarp")
    (version "2.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/swarp/"
                           "swarp-" version ".tar.gz"))
       (sha256
        (base32 "1i670waqp54vin1cn08mqckcggm9zqd69nk7yya2vvqpdizn6jpm"))))
    (build-system gnu-build-system)
    (home-page "https://www.astromatic.net/software/swarp")
    (synopsis "FITS image resampling and co-addition")
    (description
     "SWarp is a program that resamples and co-adds together FITS images using
any arbitrary astrometric projection defined in the WCS standard.")
    (license license:gpl3+)))

(define-public celestia
  (let ((commit "9dbdf29c4ac3d20afb2d9a80d3dff241ecf81dce"))
    (package
      (name "celestia")
      (version (git-version "1.6.1" "815" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/celestiaproject/celestia")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "00xibg87l1arzifakgj7s828x9pszcgx7x7ij88a561ig49ryh78"))))
      (build-system cmake-build-system)
      (native-inputs
       `(("perl" ,perl)
         ("libgit2" ,libgit2)
         ("pkg-config" ,pkg-config)
         ("libtool" ,libtool)
         ("gettext" ,gettext-minimal)))
      (inputs
       `(("glu" ,glu)
         ("glew" ,glew)
         ("libtheora" ,libtheora)
         ("libjpeg" ,libjpeg-turbo)
         ("libpng" ,libpng)
         ;; maybe required?
         ("mesa" ,mesa)
         ;; optional: fmtlib, Eigen3;
         ("fmt" ,fmt)
         ("eigen" ,eigen)
         ;; glut: for glut interface
         ("freeglut" ,freeglut)))
      (propagated-inputs
       `(("lua" ,lua)))
      (arguments
       `(#:configure-flags '("-DENABLE_GLUT=ON" "-DENABLE_QT=OFF")
         #:tests? #f))                            ;no tests
      (home-page "https://celestia.space/")
      (synopsis "Real-time 3D visualization of space")
      (description
       "This simulation program lets you explore our universe in three
dimensions.  Celestia simulates many different types of celestial objects.
From planets and moons to star clusters and galaxies, you can visit every
object in the expandable database and view it from any point in space and
time.  The position and movement of solar system objects is calculated
accurately in real time at any rate desired.")
      (license license:gpl2+))))

(define-public celestia-gtk
  (package
    (inherit celestia)
    (name "celestia-gtk")
    (inputs
     (append (alist-delete "freeglut" (package-inputs celestia))
             `(("gtk2" ,gtk+-2)
               ("gtkglext" ,gtkglext))))
    (arguments
     `(#:configure-flags '("-DENABLE_GTK=ON" "-DENABLE_QT=OFF")
       #:tests? #f))))

(define-public libnova
  (package
    (name "libnova")
    (version "0.16")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.code.sf.net/p/libnova/libnova.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0icwylwkixihzni0kgl0j8dx3qhqvym6zv2hkw2dy6v9zvysrb1b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-git-version
           (lambda _
             (substitute* "./git-version-gen"
               (("/bin/sh") (which "sh")))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (synopsis "Celestial mechanics, astrometry and astrodynamics library")
    (description "Libnova is a general purpose, double precision, Celestial
Mechanics, Astrometry and Astrodynamics library.")
    (home-page "http://libnova.sourceforge.net/")
    (license (list license:lgpl2.0+
                   license:gpl2+)))) ; examples/transforms.c & lntest/*.c

(define-public libskry
  (package
    (name "libskry")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GreatAttractor/libskry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14kwng0j8wqzlb0gqg3ayq36l15dpz7kvxc56fa47j55b376bwh6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list
        (string-append
         "LIBAV_INCLUDE_PATH=" (assoc-ref %build-inputs "ffmpeg") "/include"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ;; no configure provided
         (delete 'check) ;; no tests provided
         (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (copy-recursively "bin" lib)
               (copy-recursively "include" include))
             #t)))))
    (inputs
     `(("ffmpeg" ,ffmpeg)))
    (home-page "https://github.com/GreatAttractor/libskry")
    (synopsis "Astronimical lucky imaging library")
    (description
     "@code{libskry} implements the lucky imaging principle of astronomical
imaging: creating a high-quality still image out of a series of many thousands)
low quality ones")
    (license license:gpl3+)))

(define-public libpasastro
  ;; NOTE: (Sharlatan-20210122T215921+0000): the version tag has a build
  ;; error on spice which is resolved with the latest commit.
  (let ((commit "e3c218d1502a18cae858c83a9a8812ab197fcb60")
        (revision "1"))
    (package
      (name "libpasastro")
      (version (git-version "1.4.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pchev/libpasastro")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0asp2sn34nds5va2ghppwc41vb6j3d1mf049j949rgrll817kx47"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:make-flags
         (list
          ,(match (or (%current-target-system) (%current-system))
             ((or "aarch64-linux" "armhf-linux" "i686-linux" "x86_64-linux")
              "OS_TARGET=linux")
             (_ #f))
          ,(match (or (%current-target-system) (%current-system))
             ("i686-linux" "CPU_TARGET=i386")
             ("x86_64-linux" "CPU_TARGET=x86_64")
             ((or "armhf-linux" "aarch64-linux") "CPU_TARGET=armv7l")
             (_ #f))
          (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))
      (home-page "https://github.com/pchev/libpasastro")
      (synopsis "Interface to astronomy library for use from Pascal program")
      (description
       "This package provides shared libraries to interface Pascal program with
standard astronomy libraries:

@itemize
@item @code{libpasgetdss.so}: Interface with GetDSS to work with DSS images.
@item @code{libpasplan404.so}: Interface with Plan404 to compute planets position.
@item @code{libpaswcs.so}: Interface with libwcs to work with FITS WCS.
@item @code{libpasspice.so}: To work with NAIF/SPICE kernel.
@end itemize\n")
      (license license:gpl2+))))

(define-public missfits
  (package
    (name "missfits")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/missfits/"
                           "missfits-" version ".tar.gz"))
       (sha256
        (base32 "04jrd7fsvzr14vdmwgj2f6v97gdcfyjyz6jppml3ghr9xh12jxv5"))))
    (build-system gnu-build-system)
    (home-page "https://www.astromatic.net/software/missfits")
    (synopsis "FITS files Maintenance program")
    (description
     "MissFITS is a program that performs basic maintenance and packaging tasks
on FITS files:

@itemize
@item add/edit FITS header keywords
@item split/join Multi-Extension-FITS (MEF) files
@item unpack/pack FITS data-cubes
@item create/check/update FITS checksums, using R. Seaman's protocol
      (see http://www.adass.org/adass/proceedings/adass94/seamanr.html)
@end itemize\n")
    (license license:gpl3+)))

(define-public xplanet
  (package
    (name "xplanet")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://sourceforge/xplanet/xplanet/"
         version "/xplanet-" version ".tar.gz"))
       (sha256
        (base32 "1rzc1alph03j67lrr66499zl0wqndiipmj99nqgvh9xzm1qdb023"))
       (patches
        (search-patches
         "xplanet-1.3.1-cxx11-eof.patch"
         "xplanet-1.3.1-libdisplay_DisplayOutput.cpp.patch"
         "xplanet-1.3.1-libimage_gif.c.patch"
         "xplanet-1.3.1-xpUtil-Add2017LeapSecond.cpp.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libice" ,libice)
       ("freetype" ,freetype)
       ("pango" ,pango)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("netpbm" ,netpbm)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags
       (let ((netpbm (assoc-ref %build-inputs "netpbm")))
         (append (list
                  ;; Give correct path for pnm.h header to configure script
                  (string-append "CPPFLAGS=-I" netpbm "/include/netpbm")
                  ;; no nasa jpl cspice support
                  "--without-cspice" )))))
    (home-page "http://xplanet.sourceforge.net/")
    (synopsis "Planetary body renderer")
    (description
     "Xplanet renders an image of a planet into an X window or file.
All of the major planets and most satellites can be drawn and different map
projections are also supported, including azimuthal, hemisphere, Lambert,
Mercator, Mollweide, Peters, polyconic, orthographic and rectangular.")
    (license license:gpl2+)))

(define-public gpredict
  (package
    (name "gpredict")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/csete/gpredict/releases"
                           "/download/v" version
                           "/gpredict-" version ".tar.bz2"))
       (sha256
        (base32 "0hwf97kng1zy8rxyglw04x89p0bg07zq30hgghm20yxiw2xc8ng7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("glib" ,glib)
       ("goocanvas" ,goocanvas)
       ("gtk+" ,gtk+)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; Remove reference to non-existent file.
             (substitute* "po/POTFILES.in"
               (("src/gtk-sat-tree\\.c")
                ""))
             #t)))))
    (synopsis "Satellite tracking and orbit prediction application")
    (description
     "Gpredict is a real-time satellite tracking and orbit prediction
application.  It can track a large number of satellites and display their
position and other data in lists, tables, maps, and polar plots (radar view).
Gpredict can also predict the time of future passes for a satellite, and
provide you with detailed information about each pass.")
    (home-page "http://gpredict.oz9aec.net/index.php")
    (license license:gpl2+)))

(define-public indi
  (package
    (name "indi")
    (version "1.8.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/indilib/indi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nw4b2cdsg244slcm3yf1v11jlxbbjrpvi6ax90svs7rlandz8jv"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list
          "-DINDI_BUILD_UNITTESTS=ON"
          "-DCMAKE_BUILD_TYPE=Release"
          (string-append "-DCMAKE_INSTALL_PREFIX=" out)
          (string-append "-DUDEVRULES_INSTALL_DIR=" out "/lib/udev/rules.d")))
       #:phases
       (modify-phases %standard-phases
         (replace  'check
           (lambda _
             (chdir "test")
             (invoke "ctest")
             (chdir "..")
             #t))
         (add-before 'install 'set-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib/udev/rules.d")))
             #t)))))
    (native-inputs
     `(("googletest" ,googletest)))
    (inputs
     `(("cfitsio" ,cfitsio)
       ("curl" ,curl)
       ("fftw" ,fftw)
       ("gsl" ,gsl)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libnova" ,libnova)
       ("libtiff" ,libtiff)
       ("libusb" ,libusb)
       ("zlib" ,zlib)))
    (home-page "https://www.indilib.org")
    (synopsis "Library for astronimical intrumentation control")
    (description
     "INDI (Instrument-Neutral Device Interface) is a distributed XML-based
control protocol designed to operate astronomical instrumentation.  INDI is
small, flexible, easy to parse, scalable, and stateless.  It supports common
DCS functions such as remote control, data acquisition, monitoring, and a lot
more.")
    (license (list license:bsd-3
                   license:gpl2+
                   license:lgpl2.0+
                   license:lgpl2.1+))))

(define-public python-jplephem
  (package
    (name "python-jplephem")
    (version "2.15")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jplephem" version))
       (sha256
        (base32 "1ca3dswsslij79qg6dcijjz4l0fj6nzmxld8z93v45ahlkhps0g0"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "PYTHONPATH"
                       (string-append "./build/lib:"
                                      (getenv "PYTHONPATH")))
               (setenv "PATH" (string-append out "/bin:"
                                             (getenv "PATH")))
               (invoke "python" "-m" "unittest" "discover" "-s" "test")))))))
    (inputs
     `(("python-numpy" ,python-numpy)))
    (home-page "https://github.com/brandon-rhodes/python-jplephem")
    (synopsis "Python version of NASA DE4xx ephemerides")
    (description
     "The package is a Python implementation of the mathematics that standard
JPL ephemerides use to predict raw (x,y,z) planetary positions.")
    (license license:expat)))

(define-public python-pyerfa
  (package
    (name "python-pyerfa")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyerfa" version))
       (sha256
        (base32 "1s78mdyrxha2jcckfs0wg5ynkf0pwh1bw9mmh99vprinxh9n4xri"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled submodule library.
           (delete-file-recursively "liberfa")
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-system-liberfa
           (lambda _
             (setenv "PYERFA_USE_SYSTEM_LIBERFA" "1")
             #t)))))
    (native-inputs
     `(("pytest" ,python-pytest)
       ("setuptools-scm" ,python-setuptools-scm)
       ("pytest-doctestplus" ,python-pytest-doctestplus)))
    (inputs
     `(("liberfa" ,erfa)
       ("numpy" ,python-numpy)))
    (home-page "https://github.com/liberfa/pyerfa")
    (synopsis "Python bindings for ERFA")
    (description
     "PyERFA is the Python wrapper for the ERFA library (Essential
Routines for Fundamental Astronomy), a C library containing key algorithms for
astronomy, which is based on the SOFA library published by the International
Astronomical Union (IAU).  All C routines are wrapped as Numpy universal
functions, so that they can be called with scalar or array inputs.")
    (license license:bsd-3)))

(define-public python-sep
  (package
    (name "python-sep")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sep" version))
       (sha256
        (base32 "0wxdqn92q1grv8k7xi7h88ac6wnznw4xh5bdlz1vz6za2dgsyj4m"))))
    (build-system python-build-system)
    (native-inputs
     `(("cython" ,python-cython)
       ("pytest" ,python-pytest)))
    (inputs
     `(("numpy" ,python-numpy)))
    (home-page "https://github.com/kbarbary/sep")
    (synopsis "Astronomical source extraction and photometry library")
    (description
     "SEP makes the core algorithms of Source Extractor available as a library
of stand-alone functions and classes.")
    (license (list license:bsd-3
                   license:expat
                   license:lgpl3+))))

(define-public python-asdf
  (package
    (name "python-asdf")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf" version))
       (sha256
        (base32 "1y47zhkd90i8wmm2i35amfl0rvjqlb3fcx90xp7n9kr2z0byzyzg"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: (Sharlatan-20210207T165820+0000): Tests depend on astropy, astropy
     ;; depends on asdf. Disable circular dependence.
     `(#:tests? #f))
    (native-inputs
     `(("packaging" ,python-packaging)
       ("semantic-version" ,python-semantic-version)
       ("setuptools-scm" ,python-setuptools-scm)))
     (inputs
      `(("importlib-resources" ,python-importlib-resources)
        ("jsonschema" ,python-jsonschema)
        ("numpy" ,python-numpy)
        ("pyyaml" ,python-pyyaml)))
     (home-page "https://github.com/asdf-format/asdf")
     (synopsis "Python tools to handle ASDF files")
     (description
      "The Advanced Scientific Data Format (ASDF) is a next-generation
interchange format for scientific data.  This package contains the Python
implementation of the ASDF Standard.")
     (license license:bsd-3)))

(define-public python-astroalign
  (package
    (name "python-astroalign")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astroalign" version))
       (sha256
        (base32 "19qzv3552lgrd9qmj0rxs51wmx485hw04cbf76ds5pin85kfaiy1"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: (Sharlatan-20210213T162940+0000): I could not make tests run
     `(#:tests? #f))
    (inputs
     `(("numpy" ,python-numpy)
       ("scikit-image" ,python-scikit-image)
       ("scipy" ,python-scipy)
       ("sep" ,python-sep)))
    (home-page "https://astroalign.readthedocs.io/")
    (synopsis "Astrometric Alignment of Images")
    (description
     "ASTROALIGN is a python module that will try to align two stellar
astronomical images, especially when there is no WCS information available.")
    (license license:expat)))

(define-public python-skyfield
  (package
    (name "python-skyfield")
    (version "1.36")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "skyfield" version))
       (sha256
        (base32 "1dm1327a4qv3klj9blrvddbhl72v1fqz52ym9km8qjj9vdkpywh6"))))
    (build-system python-build-system)
    (arguments
     ;; NOTE: (Sharlatan-20210207T163305+0000): tests depend on custom test
     ;; framework https://github.com/brandon-rhodes/assay
     `(#:tests? #f))
    (inputs
     `(("certifi" ,python-certifi)
       ("jplephem" ,python-jplephem)
       ("numpy" ,python-numpy)
       ("sgp4" ,python-sgp4)))
    (home-page "https://rhodesmill.org/skyfield/")
    (synopsis "Astronomy for Python")
    (description
     "Skyfield computes positions for the stars, planets, and satellites in
orbit around the Earth.")
    (license license:expat)))
