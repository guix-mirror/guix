;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
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

(define-module (gnu packages electronics)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt))

(define-public libserialport
  (package
    (name "libserialport")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/libserialport/libserialport-"
                    version ".tar.gz"))
              (sha256
               (base32
                "17ajlwgvyyrap8z7f16zcs59pksvncwbmd3mzf98wj7zqgczjaja"))))
    (build-system gnu-build-system)
    (home-page "https://sigrok.org/wiki/Libserialport")
    (synopsis "Library for using serial ports")
    (description "Libserialport is a minimal shared library written in C that is intended
to take care of the OS-specific details when writing software that uses serial ports.")
    (license license:lgpl3+)))

(define-public libsigrokdecode
  (package
    (name "libsigrokdecode")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/libsigrokdecode/libsigrokdecode-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1h1zi1kpsgf6j2z8j8hjpv1q7n49i3fhqjn8i178rka3cym18265"))))
    (outputs '("out" "doc"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-doc
           (lambda _
             (invoke "doxygen")
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "doxy/html-api"
                               (string-append (assoc-ref outputs "doc")
                                              "/share/doc/libsigrokdecode"))
             #t)))))
    (native-inputs
     `(("check" ,check-0.14)
       ("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)))
    ;; libsigrokdecode.pc lists "python" in Requires.private, and "glib" in Requires.
    (propagated-inputs
     `(("glib" ,glib)
       ("python" ,python)))
    (build-system gnu-build-system)
    (home-page "https://www.sigrok.org/wiki/Libsigrokdecode")
    (synopsis "Library providing (streaming) protocol decoding functionality")
    (description "Libsigrokdecode is a shared library written in C, which provides
(streaming) protocol decoding functionality.")
    (license license:gpl3+)))

(define-public sigrok-firmware-fx2lafw
  (package
    (name "sigrok-firmware-fx2lafw")
    (version "0.1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/sigrok-firmware-fx2lafw/"
                    "sigrok-firmware-fx2lafw-" version ".tar.gz"))
              (sha256
               (base32
                "0fyfd82mvrcf55v5a3afq1mh1kfswk4c37qrbln6x92jm3b41x53"))))
    (arguments
     `(#:implicit-inputs? #f))
    (native-inputs
     `(("awk" ,gawk)
       ("bash" ,bash)
       ("coreutils" ,coreutils)
       ("grep" ,grep)
       ("gzip" ,gzip)
       ("make" ,gnu-make)
       ("sdcc" ,sdcc)
       ("sed" ,sed)
       ("tar" ,tar)))
    (build-system gnu-build-system)
    (home-page "https://www.sigrok.org/wiki/Fx2lafw")
    (synopsis "Firmware for Cypress FX2 chips")
    (description "Fx2lafw is free firmware for Cypress FX2 chips which makes them usable
as simple logic analyzer and/or oscilloscope hardware.")
    (license license:gpl2+)))

(define-public libsigrok
  (package
    (name "libsigrok")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/libsigrok/libsigrok-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0g6fl684bpqm5p2z4j12c62m45j1dircznjina63w392ns81yd2d"))))
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f                      ; tests need USB access
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'change-udev-group
           (lambda _
             (substitute* (find-files "contrib" "\\.rules$")
               (("plugdev") "dialout"))
             #t))
         (add-after 'build 'build-doc
           (lambda _
             (invoke "doxygen")))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "doxy/html-api"
                               (string-append (assoc-ref outputs "doc")
                                              "/share/doc/libsigrok"))
             #t))
         (add-after 'install-doc 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (rules (string-append out "/lib/udev/rules.d/")))
               (for-each (lambda (file)
                           (install-file file rules))
                         (find-files "contrib" "\\.rules$"))
               #t)))
         (add-after 'install-udev-rules 'install-fw
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((fx2lafw (assoc-ref inputs "sigrok-firmware-fx2lafw"))
                    (out (assoc-ref outputs "out"))
                    (dir-suffix "/share/sigrok-firmware/")
                    (input-dir (string-append fx2lafw dir-suffix))
                    (output-dir (string-append out dir-suffix)))
               (for-each
                (lambda (file)
                  (install-file file output-dir))
                (find-files input-dir ".")))
             #t)))))
    (native-inputs
     `(("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("sigrok-firmware-fx2lafw" ,sigrok-firmware-fx2lafw)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python)
       ("zlib" ,zlib)))
    ;; libsigrokcxx.pc lists "glibmm" in Requires
    ;; libsigrok.pc lists "libserialport", "libusb", "libftdi" and "libzip" in
    ;; Requires.private and "glib" in Requires
    (propagated-inputs
     `(("glib" ,glib)
       ("glibmm" ,glibmm)
       ("libserialport" ,libserialport)
       ("libusb" ,libusb)
       ("libftdi" ,libftdi)
       ("libzip" ,libzip)))
    (build-system gnu-build-system)
    (home-page "https://www.sigrok.org/wiki/Libsigrok")
    (synopsis "Library which provides the basic hardware access drivers for logic
analyzers")
    (description "@code{libsigrok} is a shared library written in C which provides the basic hardware
access drivers for logic analyzers and other supported devices, as well as input/output file
format support.")
    (license license:gpl3+)))

(define-public sigrok-cli
  (package
    (name "sigrok-cli")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/sigrok-cli/sigrok-cli-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1f0a2k8qdcin0pqiqq5ni4khzsnv61l21v1dfdjzayw96qzl9l3i"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("libsigrok" ,libsigrok)
       ("libsigrokdecode" ,libsigrokdecode)))
    (build-system gnu-build-system)
    (home-page "https://sigrok.org/wiki/Sigrok-cli")
    (synopsis "Command-line frontend for sigrok")
    (description "Sigrok-cli is a command-line frontend for sigrok.")
    (license license:gpl3+)))

(define-public pulseview
  (package
    (name "pulseview")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://sigrok.org/download/source/pulseview/pulseview-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1jxbpz1h3m1mgrxw74rnihj8vawgqdpf6c33cqqbyd8v7rxgfhph"))
              (patches (search-patches "pulseview-qt515-compat.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DENABLE_TESTS=y")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-empty-doc-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion (string-append out "/share")
                 ;; Use RMDIR to never risk silently deleting files.
                 (rmdir "doc/pulseview")
                 (rmdir "doc"))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))
    (inputs
     `(("boost" ,boost)
       ("glib" ,glib)
       ("glibmm" ,glibmm)
       ("libsigrok" ,libsigrok)
       ("libsigrokdecode" ,libsigrokdecode)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (home-page "https://www.sigrok.org/wiki/PulseView")
    (synopsis "Qt based logic analyzer, oscilloscope and MSO GUI for sigrok")
    (description "PulseView is a Qt based logic analyzer, oscilloscope and MSO GUI
for sigrok.")
    (license license:gpl3+)))

(define-public comedilib
  (package
    (name "comedilib")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.comedi.org/download/comedilib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0wzh23iyix4xj211fsd8hwrdcjhg2w5jswk9kywb1gpd3h8afajj"))))
    (build-system gnu-build-system)
    (synopsis "Library for Comedi")
    (description "Comedilib is a user-space library that provides a
developer-friendly interface to Comedi devices.  Comedi is a collection of
drivers for a variety of common data acquisition plug-in boards.  The drivers
are implemented as a core Linux kernel module providing common functionality and
individual low-level driver modules.")
    (home-page "https://www.comedi.org/")
    (license license:lgpl2.1)))

(define-public xoscope
  (package
    (name "xoscope")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xoscope/xoscope/"
                                  version "/xoscope-" version ".tar.gz"))
              (sha256
               (base32
                "1b9wxnrwz8qy6qyx5icrklb4720rlxnr1c4h3dr6g0dzj6nkc5av"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("comedilib" ,comedilib)
       ("fftw" ,fftw)
       ("gtk+-2" ,gtk+-2)
       ("gtkdatabox" ,gtkdatabox)))
    (synopsis "Digital oscilloscope")
    (description "Xoscope is a digital oscilloscope that can acquire signals
from ALSA, ESD, and COMEDI sources.  This package currently does not include
support for ESD sources.")
    (home-page "http://xoscope.sourceforge.net/")
    (license license:gpl2+)))
