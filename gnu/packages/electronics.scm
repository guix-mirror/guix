;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdcc))

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
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/libsigrokdecode/libsigrokdecode-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1w434nl1syjkvwl08lji3r9sr60lbxp1nqys8hqwzv2lgiwrx3g0"))))
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
     `(("check" ,check)
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
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/sigrok-firmware-fx2lafw/"
                    "sigrok-firmware-fx2lafw-" version ".tar.gz"))
              (sha256
               (base32
                "18x5zj3xbcylvb651dia6n4zxbdnv0j62r5af60d0l2g68knkjg4"))))
    (native-inputs
     `(("sdcc" ,sdcc)))
    (build-system gnu-build-system)
    (home-page "https://www.sigrok.org/wiki/Fx2lafw")
    (synopsis "Firmware for Cypress FX2 chips")
    (description "Fx2lafw is free firmware for Cypress FX2 chips which makes them usable
as simple logic analyzer and/or oscilloscope hardware.")
    (license license:gpl2+)))

(define-public libsigrok
  (package
    (name "libsigrok")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/libsigrok/libsigrok-"
                    version ".tar.gz"))
              (sha256
               (base32
                "171b553dir5gn6w4f7n37waqk62nq2kf1jykx4ifjacdz5xdw3z4"))))
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
     `(("check" ,check)
       ("doxygen" ,doxygen)
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
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/sigrok-cli/sigrok-cli-"
                    version ".tar.gz"))
              (sha256
               (base32
                "15vpn1psriadcbl6v9swwgws7dva85ld03yv6g1mgm27kx11697m"))))
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
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/pulseview/pulseview-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0bvgmkgz37n2bi9niskpl05hf7rsj1lj972fbrgnlz25s4ywxrwy"))))
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
     `(("pkg-config" ,pkg-config)))
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
