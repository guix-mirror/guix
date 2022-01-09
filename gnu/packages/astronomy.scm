;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 by Amar Singh <nly@disroot.org>
;;; Copyright © 2020 R Veera Kumar <vkor@vkten.in>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
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
  #:use-module (gnu packages boost)
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
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages time)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
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

(define-public python-fitsio
  (package
    (name "python-fitsio")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fitsio" version))
       (sha256
        (base32 "1llql2i6xr9lkdl81jx5nvz80kspamvira90546y32ldy551hq1l"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove the bundled cfitsio
        `(begin
           (delete-file-recursively "cfitsio3490")
           (substitute* "MANIFEST.in"
             (("recursive-include cfitsio3490.*$\n") ""))
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-cfitsio
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((cfitsio (assoc-ref inputs "cfitsio"))
                    (includedir (string-append "\"" cfitsio "/include\""))
                    (libdir (string-append "\"" cfitsio "/lib\"")))
               ;; Use Guix' cfitsio instead of the bundled one
               (substitute* "setup.py"
                 (("self.use_system_fitsio = False") "pass")
                 (("self.system_fitsio_includedir = None") "pass")
                 (("self.system_fitsio_libdir = None") "pass")
                 (("self.use_system_fitsio") "True")
                 (("self.system_fitsio_includedir") includedir)
                 (("self.system_fitsio_libdir") libdir)))))
         (add-after 'unpack 'skip-bzip2-test
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The bzip2 test fails because Guix' cfitsio
             ;; is built without bzip2 support.
             (substitute* "fitsio/test.py"
               (("'SKIP_BZIP_TEST' in os.environ") "True")))))))
    (propagated-inputs
     (list python-numpy cfitsio))
    (home-page "https://github.com/esheldon/fitsio")
    (synopsis
     "Python library to read from and write to FITS files")
    (description
     "This package provides a Python library for reading from and writing
to @acronym{FITS, Flexible Image Transport System} files using the
CFITSIO library.  Among other things, it can

@itemize
@item read and write image, binary, and ascii table extensions;

@item read arbitrary subsets of tables in a lazy manner;

@item query the rows and columns of a table;

@item read and write header keywords;

@item read and write Gzip files.
@end itemize")
    (license license:gpl2+)))

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
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/liberfa/erfa")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s9dpj0jdkqcg552f00jhd722czji4pffabmpys5pgi6djckq4f4"))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake autoconf libtool pkg-config))
    (home-page "https://github.com/liberfa/erfa")
    (synopsis "Essential Routines for Fundamental Astronomy")
    (description
     "The @acronym{ERFA, Essential Routines for Fundamental Astronomy} C library
contains key algorithms for astronomy, and is based on the @acronym{SOFA,
Standards of Fundamental Astronomy} library published by the @acronym{IAU,
International Astronomical Union}.")
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
     (list cfitsio))
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
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnuastro/gnuastro-"
                           version ".tar.lz"))
       (sha256
        (base32
         "07xr0r5dmjpnrz7ylf7k3vmjjna2jafi16lfvkqwxj9fyssmz207"))))
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
     (list libtool lzip))
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
     (list autoconf automake libtool))
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
     (list pkg-config))
     (inputs
      (list gtkmm-3 libskry ffmpeg))
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
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Stellarium/stellarium"
                           "/releases/download/v" version
                           "/stellarium-" version ".tar.gz"))
       (sha256
        (base32 "049jlc8vx06pad5h2syrmf7f1l346yr5iraai0wkn8s8pk30j8q7"))))
    (build-system cmake-build-system)
    (inputs
     (list qtbase-5
           qtlocation
           qtmultimedia
           qtscript
           qtserialport
           zlib))
    (native-inputs
     `(("gettext" ,gettext-minimal)     ; xgettext is used at compile time
       ("perl" ,perl)                   ; for pod2man
       ("qtbase" ,qtbase-5)               ; Qt MOC is needed at compile time
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
         ("fmt" ,fmt-7)
         ("eigen" ,eigen)
         ;; glut: for glut interface
         ("freeglut" ,freeglut)))
      (propagated-inputs
       (list lua))
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

(define-public python-astropy
  (package
    (name "python-astropy")
    (version "5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy" version))
       (sha256
        (base32 "09rr9z2kn5qw34fqpwxgcwsn9m5aw6f0dd0pm232aa8k3qakw83h"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove Python bundles.
           (with-directory-excursion "astropy/extern"
             (for-each delete-file-recursively '("ply" "configobj")))
           ;; Remove cextern bundles and leave the wcslib bundle.  Astropy
           ;; upgrades to different versions of wcslib every few releases
           ;; and tests break every upgrade.
           ;; TODO: unbundle wcslib.
           (with-directory-excursion "cextern"
             (for-each delete-file-recursively '("cfitsio" "expat")))
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'preparations
           (lambda _
             ;; Use our own libraries in place of bundles, with the
             ;; exception of wcslib.
             (setenv "ASTROPY_USE_SYSTEM_CFITSIO" "1")
             (setenv "ASTROPY_USE_SYSTEM_EXPAT" "1")
             ;; Some tests require a writable home.
             (setenv "HOME" "/tmp")
             ;; Relax xfail tests.
             (substitute* "setup.cfg"
               (("xfail_strict = true") "xfail_strict = false"))
             ;; Replace all references to external ply.
             (let ((ply-files '("coordinates/angle_formats.py"
                                "utils/parsing.py")))
               (with-directory-excursion "astropy"
                 (map (lambda (file)
                        (substitute* file (("astropy.extern.ply")
                                           "ply")))
                      ply-files)))
             ;; Replace reference to external configobj.
             (with-directory-excursion "astropy/config"
               (substitute* "configuration.py"
                 (("from astropy.extern.configobj ") "")))))
         ;; This file is opened in both install and check phases.
         (add-before 'install 'writable-compiler
           (lambda _ (make-file-writable "astropy/_compiler.c")))
         (add-before 'check 'writable-compiler
           (lambda _ (make-file-writable "astropy/_compiler.c")))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               ;; Extensions have to be rebuilt before running the tests.
               (invoke "python" "setup.py" "build_ext" "--inplace")
               (invoke "python" "-m" "pytest" "--pyargs" "astropy"
                       ;; Skip tests that need remote data.
                       "-m" "not remote_data")))))))
    (native-inputs
     (list pkg-config
           python-coverage
           python-cython
           python-extension-helpers
           python-ipython
           python-jplephem
           python-objgraph
           python-pytest
           python-pytest-astropy
           python-pytest-xdist
           python-setuptools-scm
           python-sgp4
           python-skyfield))
    (inputs
     (list cfitsio expat))
    (propagated-inputs
     (list python-configobj
           python-numpy
           python-packaging
           python-ply
           python-pyerfa
           python-pyyaml))
    (home-page "https://www.astropy.org/")
    (synopsis "Core package for Astronomy in Python")
    (description
     "Astropy is a single core package for Astronomy in Python.  It contains
much of the core functionality and some common tools needed for performing
astronomy and astrophysics.")
    (license license:bsd-3)))

(define-public python-astropy-healpix
  (package
    (name "python-astropy-healpix")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy-healpix" version))
       (sha256
        (base32 "1436ml03xkmvx4afzbhfj67ab91418sz1w3lq1b18r43qchnd6j0"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This file is opened in both install and check phases.
         (add-before 'install 'writable-compiler
           (lambda _ (make-file-writable "astropy_healpix/_compiler.c")))
         (add-before 'check 'writable-compiler
           (lambda _ (make-file-writable "astropy_healpix/_compiler.c")))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               ;; Extensions have to be rebuilt before running the tests.
               (invoke "python" "setup.py" "build_ext" "--inplace")
               (invoke "python" "-m" "pytest"
                       "--pyargs" "astropy_healpix")))))))
    (native-inputs
     (list python-extension-helpers
           python-hypothesis
           python-pytest-astropy
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy python-numpy))
    (home-page "https://github.com/astropy/astropy-healpix")
    (synopsis "HEALPix for Astropy")
    (description "This package provides HEALPix to the Astropy project.")
    (license license:bsd-3)))

(define-public python-astroquery
  (package
    (name "python-astroquery")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astroquery" version))
       (sha256
        (base32 "06xy0qzqmps6z5vwfkh5fkhr151p7g94r2j0mvp1rc8zns22y010"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'writable-home
           (lambda _                    ; some tests need a writable home
             (setenv "HOME" (getcwd))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest" "--pyargs" "astroquery"
                       ;; Skip tests that require online data.
                       "-m" "not remote_data")))))))
    (propagated-inputs
     (list python-astropy
           python-beautifulsoup4
           python-html5lib
           python-keyring
           python-numpy
           python-pyvo
           python-requests))
    (native-inputs
     (list python-flask
           python-jinja2
           python-matplotlib
           python-pytest-astropy
           python-pytest-dependency))
    (home-page "https://www.astropy.org/astroquery/")
    (synopsis "Access online astronomical data resources")
    (description "Astroquery is a package that contains a collection of tools
to access online Astronomical data.  Each web service has its own sub-package.")
    (license license:bsd-3)))

(define-public python-photutils
  (package
    (name "python-photutils")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "photutils" version))
       (sha256
        (base32 "1a8djakaya6w5iv9237gkcz39brqzgrfs2wqrl0izi1s85cfdymn"))))
    (build-system python-build-system)
    (arguments
     `(#:test-target "pytest"
       #:phases
       (modify-phases %standard-phases
         ;; This file is opened in both install and check phases.
         (add-before 'install 'writable-compiler
           (lambda _ (make-file-writable "photutils/_compiler.c")))
         (add-before 'check 'writable-compiler
           (lambda _ (make-file-writable "photutils/_compiler.c"))))))
    (propagated-inputs
     (list python-astropy python-numpy))
    (native-inputs
     (list python-cython
           python-extension-helpers
           python-pytest-astropy
           python-pytest-runner
           python-setuptools-scm))
    (home-page "https://github.com/astropy/photutils")
    (synopsis "Source detection and photometry")
    (description "Photutils is an Astropy package for detection and photometry
of astronomical sources.")
    (license license:bsd-3)))

(define-public python-pyvo
  (package
    (name "python-pyvo")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyvo" version))
       (sha256
        (base32 "1lap703wxbyxqlbk85myirp4pkdnc6cg10xhfajfsvz5k0hm5ffw"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest" "--pyargs" "pyvo" "-k"
                       (string-append   ; these tests use the network
                        "not test_access_with_string"
                        " and not test_access_with_list"
                        " and not test_access_with_expansion"))))))))
    (native-inputs
     (list python-pytest-astropy python-requests-mock))
    (propagated-inputs
     (list python-astropy python-mimeparse python-pillow python-requests))
    (home-page "https://github.com/astropy/pyvo")
    (synopsis "Access Virtual Observatory data and services")
    (description
     "PyVO is a package providing access to remote data and services of the
Virtual observatory (VO) using Python.")
    (license license:bsd-3)))

(define-public python-regions
  (package
    (name "python-regions")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "regions" version))
       (sha256
        (base32 "1bjrcjchbw3xw1a26d5g198lh7vxpp9m5sal58r7f8mmr1d8g2dc"))))
    (build-system python-build-system)
    (arguments
     `(#:test-target "pytest"
       #:phases
       (modify-phases %standard-phases
         ;; This doctest requires online data.
         (add-after 'unpack 'delete-doctest
           (lambda _ (delete-file "docs/masks.rst")))
         ;; This file is opened in both install and check phases.
         (add-before 'install 'writable-compiler
           (lambda _ (make-file-writable "regions/_compiler.c")))
         (add-before 'check 'writable-compiler
           (lambda _ (make-file-writable "regions/_compiler.c")))
         (add-before 'check 'writable-home
           (lambda _  (setenv "HOME" (getcwd)))))))
    (propagated-inputs
     (list python-astropy python-numpy))
    (native-inputs
     (list python-cython
           python-extension-helpers
           python-pytest-arraydiff
           python-pytest-astropy
           python-pytest-runner
           python-setuptools-scm))
    (home-page "https://github.com/astropy/regions")
    (synopsis "Package for region handling")
    (description "Regions is an Astropy package for region handling.")
    (license license:bsd-3)))

(define-public python-astral
  (package
    (name "python-astral")
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astral" version))
       (sha256
        (base32 "1gkggdibccmdy9glymw3kbrkzm6svvsg0lk56hhy92y4smkrj7g4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest")))))))
    (native-inputs
     (list python-freezegun python-setuptools-scm))
    (propagated-inputs
     (list python-dataclasses python-pytest python-pytz))
    (home-page "https://github.com/sffjunkie/astral")
    (synopsis "Calculations for the position of the sun and moon")
    (description "Astral is a Python module that calculates times for various
positions of the sun: dawn, sunrise, solar noon, sunset, dusk, solar
elevation, solar azimuth, rahukaalam, and the phases of the moon.")
    (license license:asl2.0)))

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
     (list autoconf automake libtool))
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
     (list ffmpeg))
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
     (list pkg-config))
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
     (list curl glib goocanvas gtk+))
    (arguments
     `(#:configure-flags '("CFLAGS=-O2 -g -fcommon")
       #:phases
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

(define-public sgp4
  ;; No tagged releases, use commit directly.
  (let ((commit "ca9d4d97af4ee62461de6f13e0c85d1dc6000040")
        (revision "1"))
    (package
      (name "sgp4")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dnwrnr/sgp4")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1xwfa6papmd2qz5w0hwzvijmzvp9np8dlw3q3qz4bmsippzjv8p7"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               ;; Tests fails, probably because of a few "(e <= -0.001)" errors.
               ;; Or maybe this is not the right way to run the tests?
               ;; (invoke "runtest/runtest")
               #t)))))
      (home-page "https://github.com/dnwrnr/sgp4")
      (synopsis "Simplified perturbations models library")
      (description
       "This is a library implementing the simplified perturbations model.
It can be used to calculate the trajectory of satellites.")
      (license license:asl2.0))))

(define-public imppg
  (package
    (name "imppg")
    (version "0.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GreatAttractor/imppg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04synbmyz0hkipl1cdc26nr42r57v494yjw8pi4jx0jrxrawgj9h"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;no test provided
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p "build")
             (chdir "build")
             (invoke
              "cmake"
              "-G" "Unix Makefiles"
              "-DCMAKE_BUILD_TYPE=Release"
              (string-append "-DCMAKE_INSTALL_PREFIX=" (assoc-ref outputs "out"))
              ".."))))))
    (native-inputs
     (list boost pkg-config))
    (inputs
     (list cfitsio freeimage glew wxwidgets))
    (home-page "https://github.com/GreatAttractor/imppg")
    (synopsis "Astronomical Image Post-Proccessor (ImPPG)")
    (description
     "ImPPG performs Lucy-Richardson deconvolution, unsharp masking,
brightness normalization and tone curve adjustment.  It can also apply
previously specified processing settings to multiple images.  All operations
are performed using 32-bit floating-point arithmetic.

Supported input formats: FITS, BMP, JPEG, PNG, TIFF (most of bit depths and
compression methods), TGA and more.  Images are processed in grayscale and can
be saved as: BMP 8-bit; PNG 8-bit; TIFF 8-bit, 16-bit, 32-bit
floating-point (no compression, LZW- or ZIP-compressed), FITS 8-bit, 16-bit,
32-bit floating-point.")
    (license license:gpl3+)))

(define-public indi
  (package
    (name "indi")
    (version "1.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/indilib/indi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c7md288d3g2vf0m1ai6x2l4j4rmlasc4rya92phvd4ynf8vcki2"))))
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
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "test"
                 (invoke "ctest")))))
         (add-before 'install 'set-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib/udev/rules.d"))))))))
    (native-inputs
     (list googletest))
    (inputs
     (list cfitsio
           curl
           fftw
           gsl
           libjpeg-turbo
           libnova
           libtiff
           libusb
           zlib))
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

(define-public sunclock
  (let ((commit "f4106eb0a81f7594726d6b2859efd8fc64cc1225")
        (revision "1"))
    (package
      (name "sunclock")
      (version (git-version "3.57" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nongiach/Sunclock")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1rczdpmhvfw57b9r793vq8vqlbdhlkgj52fxwrdfl6cwj95a9kv2"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         (list (string-append "DESTDIR=" %output)
               ;; Fix incorrect argument given to gcc. Error message:
               ;; "gcc: error: DefaultGcc2AMD64Opt: No such file or directory"
               "CDEBUGFLAGS=")
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda _
               (chdir "sunclock-3.57")
               (substitute* "Imakefile"
                 (("^MANDIR=/X11R6/man/man1")
                  "MANDIR=/share/man/man1")
                 (("^BINDIR=/X11R6/bin")
                  "BINDIR=/bin")
                 ;; Disable ZLIB support for vmf files because zlib implements
                 ;; `gzgetc` as a macro instead of a function, which results in
                 ;; a compilation error.
                 ((" -DZLIB") "")
                 ((" -lz") "")
                 (("cd \\$\\(DESTDIR\\)\\$\\(SHAREDIR\\)/earthmaps/vmf ; \
gzip -f \\*.vmf")
                  ""))
               ;; Generate Makefile.
               (invoke "xmkmf"))))
         #:tests? #f))  ; No check target.
      (inputs
       (list libjpeg-turbo libpng libx11 libxpm))
      (native-inputs
       (list imake))
      (home-page "https://github.com/nongiach/Sunclock")
      (synopsis
       "Map of the Earth that shows which portion is illuminated by the Sun")
      (description
       "Sunclock displays a map of the Earth and shows which portion is
illuminated by the Sun.  It can commute between two states, the \"clock window\"
and the \"map window\".  The clock window displays a small map of the Earth and
therefore occupies little space on the screen, while the \"map window\" displays
a large map and offers more advanced functions: local time of cities, Sun and
Moon position, etc.")
      (license license:gpl2+))))

(define-public python-jplephem
  (package
    (name "python-jplephem")
    (version "2.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jplephem" version))
       (sha256
        (base32 "1xvivnsywjaf5lxn3kyg2jhhq393gcwkjrl634m8dn52ypidrcdb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (let ((out (assoc-ref outputs "out")))
                 (add-installed-pythonpath inputs outputs)
                 (setenv "PATH" (string-append out "/bin:" (getenv "PATH")))
                 (invoke "python" "-m" "unittest" "discover" "-s" "test"))))))))
    (inputs
     (list python-numpy))
    (home-page "https://github.com/brandon-rhodes/python-jplephem")
    (synopsis "Python version of NASA DE4xx ephemerides")
    (description
     "The package is a Python implementation of the mathematics that standard
JPL ephemerides use to predict raw (x,y,z) planetary positions.")
    (license license:expat)))

(define-public python-pyerfa
  (package
    (name "python-pyerfa")
    (version "2.0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyerfa" version))
       (sha256
        (base32 "0c6y1rm51kj8ahbr1vwbswck3ix77dc3zhc2fkg6w7iczrzn7m1g"))
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
             (setenv "PYERFA_USE_SYSTEM_LIBERFA" "1"))))))
    (native-inputs
     (list python-pytest-doctestplus python-pytest python-setuptools-scm))
    (inputs
     (list erfa))
    (propagated-inputs
     (list python-numpy))
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
     (list python-cython python-pytest))
    (inputs
     (list python-numpy))
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
    (version "2.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf" version))
       (sha256
        (base32 "1mj52l2m8pbhiqacgjakjpvqi8kyx470yw151lcsswbq5wp0rsc6"))))
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
    (version "1.39")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "skyfield" version))
       (sha256
        (base32 "1qh3k7g9dm6idppk87hnwxpx9a22xx98vav0zk31p6291drak3as"))))
    (build-system python-build-system)
    (arguments
     ;; NOTE: (Sharlatan-20210207T163305+0000): tests depend on custom test
     ;; framework https://github.com/brandon-rhodes/assay
     `(#:tests? #f))
    (inputs
     (list python-certifi python-jplephem python-numpy python-sgp4))
    (home-page "https://rhodesmill.org/skyfield/")
    (synopsis "Astronomy for Python")
    (description
     "Skyfield computes positions for the stars, planets, and satellites in
orbit around the Earth.")
    (license license:expat)))
