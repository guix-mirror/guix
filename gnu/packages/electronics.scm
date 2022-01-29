;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages stb))

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
                "1h1zi1kpsgf6j2z8j8hjpv1q7n49i3fhqjn8i178rka3cym18265"))
              (patches
               (search-patches "libsigrokdecode-python3.9-fix.patch"))))
    (outputs '("out" "doc"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (invoke "autoconf")
             (invoke "aclocal")
             (invoke "automake" "-ac")))
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
     (list check-0.14 doxygen graphviz pkg-config automake autoconf))
    ;; libsigrokdecode.pc lists "python" in Requires.private, and "glib" in Requires.
    (propagated-inputs
     (list glib python))
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
  (let ((commit "a7e919a3a6b7fd511acbe1a280536b76c70c28d2")
        (revision "1"))
    (package
      (name "libsigrok")
      (version (git-version "0.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://sigrok.org/libsigrok")
               (commit commit)))
         (sha256
          (base32 "0km3fyv5s2byrm4zpbss2527ynhw4nb67imnbawwic2a6zh9jiyc"))
         (file-name (git-file-name name version))))
      (outputs '("out" "doc"))
      (arguments
       `(#:tests? #f                      ; tests need USB access
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'change-udev-group
             (lambda _
               (substitute* (find-files "contrib" "\\.rules$")
                 (("plugdev") "dialout"))))
           (add-after 'build 'build-doc
             (lambda _
               (invoke "doxygen")))
           (add-after 'install 'install-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (copy-recursively "doxy/html-api"
                                 (string-append (assoc-ref outputs "doc")
                                                "/share/doc/libsigrok"))))
           (add-after 'install-doc 'install-udev-rules
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out   (assoc-ref outputs "out"))
                      (rules (string-append out "/lib/udev/rules.d/")))
                 (for-each (lambda (file)
                             (install-file file rules))
                           (find-files "contrib" "\\.rules$")))))
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
                  (find-files input-dir "."))))))))
      (native-inputs
       (list autoconf automake doxygen graphviz libtool
             sigrok-firmware-fx2lafw pkg-config))
      (inputs
       (list python zlib))
      ;; libsigrokcxx.pc lists "glibmm" in Requires libsigrok.pc lists
      ;; "libserialport", "libusb", "libftdi" and "libzip" in Requires.private
      ;; and "glib" in Requires
      (propagated-inputs
       (list glib
             glibmm-2.64
             libserialport
             libusb
             libftdi
             libzip))
      (build-system gnu-build-system)
      (home-page "https://www.sigrok.org/wiki/Libsigrok")
      (synopsis "Basic hardware access drivers for logic analyzers")
      (description "@code{libsigrok} is a shared library written in C which
provides the basic hardware access drivers for logic analyzers and other
supported devices, as well as input/output file format support.")
      (license license:gpl3+))))

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
     (list pkg-config))
    (inputs
     (list glib libsigrok libsigrokdecode))
    (build-system gnu-build-system)
    (home-page "https://sigrok.org/wiki/Sigrok-cli")
    (synopsis "Command-line frontend for sigrok")
    (description "Sigrok-cli is a command-line frontend for sigrok.")
    (license license:gpl3+)))

(define-public openboardview
  (package
    (name "openboardview")
    (version "8.95.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenBoardView/OpenBoardView")
                    (commit version)
                    (recursive? #t)))   ;for the "src/imgui" submodule
              (file-name (git-file-name name version))
              (modules '((ice-9 ftw)
                         (srfi srfi-26)
                         (guix build utils)))
              (snippet
               '(with-directory-excursion "src"
                  ;; Keep the bundled ImGui for now, as in the current version
                  ;; (~1.79), it requires the glad loader generated at build
                  ;; time as an input.
                  (define keep (list "." ".." "imgui" "openboardview"))
                  (for-each (lambda (f)
                              (when (eq? 'directory (stat:type (lstat f)))
                                (delete-file-recursively f)))
                            (scandir "." (negate (cut member <> keep))))))
              (patches
               (search-patches "openboardview-use-system-utf8.patch"))
              (sha256
               (base32
                "16mrs7bimwp8a8lb2wqhfisy6j0hl9574l4h9yb66v46aglvmd3h"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:imported-modules `((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  ((guix build glib-or-gtk-build-system) #:prefix gtk:))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-timestamps
            (lambda _
              ;; The __TIMESTAMP__ CPP macro does apparently not honor
              ;; SOURCE_EPOCH_DATE.  Patch it to use __DATE__ instead, which
              ;; does (see:
              ;; https://github.com/OpenBoardView/OpenBoardView/issues/229 and
              ;; https://issues.guix.gnu.org/53647).
              (substitute* '("src/openboardview/BoardView.cpp"
                             "src/openboardview/main_opengl.cpp")
                (("__TIMESTAMP__")
                 "__DATE__"))))
          (add-before 'configure 'configure-glad
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/CMakeLists.txt"
                (("add_subdirectory\\(glad\\)")
                 (string-append
                  ;; Configure Glad to use static Khronos XML specifications
                  ;; instead of attempting to fetch them from the Internet.
                  "option(GLAD_REPRODUCIBLE \"Reproducible build\" ON)\n"
                  ;; Use the CMake files from our glad package.
                  "add_subdirectory("
                  (search-input-directory inputs "share/glad") ;source_dir
                  " src/glad)\n")))))                          ;binary dir
          (add-before 'configure 'fix-utf8-include-directive
            ;; Our utf8-h package makes the header available as "utf8.h"
            ;; directly rather than "utf8/utf8.h".
            (lambda _
              (substitute* '("src/openboardview/FileFormats/BRDFile.cpp"
                             "src/openboardview/BoardView.cpp")
                (("utf8/utf8.h") "utf8.h"))))
          (add-before 'configure 'dynamically-load-gtk-via-absolute-path
            ;; The GTK library is not linked thus not present in the RUNPATH of
            ;; the produced binary; the absolute path of the libraries must to
            ;; the dynamic loader otherwise they aren't found.
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/openboardview/unix.cpp"
                (("libgtk-3.so")
                 (search-input-file inputs "lib/libgtk-3.so")))))
          ;; Add the two extra phases from `glib-or-gtk-build-system'.
          (add-after 'install 'glib-or-gtk-compile-schemas
            (assoc-ref gtk:%standard-phases 'glib-or-gtk-compile-schemas))
          (add-after 'install 'glib-or-gtk-wrap
            (assoc-ref gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list pkg-config
           python
           glad
           stb-image
           utf8-h))
    (inputs
     (list fontconfig
           gtk+
           sdl2
           sqlite
           zlib))
    (home-page "https://openboardview.org/")
    (synopsis "Viewer for BoardView files")
    (description "OpenBoardView is a viewer for BoardView files, which present
the details of a printed circuit board (PCB).  It comes with features
such as:
@itemize
@item Dynamic part outline rendering, including complex connectors
@item Annotations, for leaving notes about parts, nets, pins or location
@item Configurable colour themes
@item Configurable DPI to facilitate usage on 4K monitors
@item Configurable for running on slower systems
@item Reads FZ (with key), BRD, BRD2, BDV and BV* formats.
@end itemize")
    (license license:expat)))

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
              (patches (search-patches "pulseview-qt515-compat.patch"
                                       "pulseview-glib-2.68.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ;format_time_minutes_test is failing
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-empty-doc-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion (string-append out "/share")
                 ;; Use RMDIR to never risk silently deleting files.
                 (rmdir "doc/pulseview")
                 (rmdir "doc"))))))))
    (native-inputs
     (list pkg-config qttools))
    (inputs
     (list boost
           glib
           glibmm
           libsigrok
           libsigrokdecode
           qtbase-5
           qtsvg))
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
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xoscope/xoscope/"
                                  version "/xoscope-" version ".tar.gz"))
              (sha256
               (base32
                "0a5ycfc1qdmibvagc82r2mhv2i99m6pndy5i6ixas3j2297g6pgq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list m4 pkg-config))
    (inputs
     (list alsa-lib comedilib fftw gtk+ gtkdatabox))
    (synopsis "Digital oscilloscope")
    (description "Xoscope is a digital oscilloscope that can acquire signals
from ALSA, ESD, and COMEDI sources.  This package currently does not include
support for ESD sources.")
    (home-page "http://xoscope.sourceforge.net/")
    (license license:gpl2+)))
