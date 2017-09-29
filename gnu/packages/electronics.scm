;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
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
    (home-page "http://sigrok.org/wiki/Libserialport")
    (synopsis "Library for using serial ports")
    (description "Libserialport is a minimal shared library written in C that is intended
to take care of the OS-specific details when writing software that uses serial ports.")
    (license license:lgpl3+)))

(define-public libsigrokdecode
  (package
    (name "libsigrokdecode")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/libsigrokdecode/libsigrokdecode-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1hfigfj1976qk11kfsgj75l20qvyq8c9p2h4mjw23d59rsg5ga2a"))))
    (outputs '("out" "doc"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-doc
           (lambda _
             (zero? (system* "doxygen"))))
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
    (home-page "http://www.sigrok.org/wiki/Libsigrokdecode")
    (synopsis "Library providing (streaming) protocol decoding functionality")
    (description "Libsigrokdecode is a shared library written in C, which provides
(streaming) protocol decoding functionality.")
    (license license:gpl3+)))

(define-public sigrok-firmware-fx2lafw
  (package
    (name "sigrok-firmware-fx2lafw")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/sigrok-firmware-fx2lafw/"
                    "sigrok-firmware-fx2lafw-" version ".tar.gz"))
              (sha256
               (base32
                "0bbdgy4rpc00jl0l0744m2ibjlqi26bhrkjr7vplivdsjdmhjx6a"))))
    (native-inputs
     `(("sdcc" ,sdcc)))
    (build-system gnu-build-system)
    (home-page "http://www.sigrok.org/wiki/Fx2lafw")
    (synopsis "Firmware for Cypress FX2 chips")
    (description "Fx2lafw is free firmware for Cypress FX2 chips which makes them usable
as simple logic analyzer and/or oscilloscope hardware.")
    (license license:gpl2+)))

(define-public libsigrok
  (package
    (name "libsigrok")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/libsigrok/libsigrok-"
                    version ".tar.gz"))
              (sha256
               (base32
                "197kr5ip98lxn7rv10zs35d1w0j7265s0xvckx0mq2l8kdvqd32c"))))
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f ; tests need usb access
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'change-udev-group
           (lambda _
             (let ((file "contrib/z60_libsigrok.rules"))
               (substitute* file
                 (("plugdev") "dialout"))
               (rename-file file "contrib/60-libsigrok.rules")
               #t)))
         (add-after 'build 'build-doc
           (lambda _
             (zero? (system* "doxygen"))))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (copy-recursively "doxy/html-api"
                               (string-append (assoc-ref outputs "doc")
                                              "/share/doc/libsigrok"))
             #t))
         (add-after 'install-doc 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "contrib/60-libsigrok.rules"
                           (string-append
                            (assoc-ref outputs "out")
                            "/lib/udev/rules.d/"))))
         (add-after 'install-udev-rules 'install-fw
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((fx2lafw (assoc-ref inputs "sigrok-firmware-fx2lafw"))
                    (out (assoc-ref outputs "out"))
                    (dir-suffix "/share/sigrok-firmware/")
                    (input-dir (string-append fx2lafw dir-suffix))
                    (output-dir (string-append out dir-suffix)))
               (mkdir-p output-dir)
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
    (home-page "http://www.sigrok.org/wiki/Libsigrok")
    (synopsis "Library which provides the basic hardware access drivers for logic
analyzers")
    (description "@code{libsigrok} is a shared library written in C which provides the basic hardware
access drivers for logic analyzers and other supported devices, as well as input/output file
format support.")
    (license license:gpl3+)))

(define-public sigrok-cli
  (package
    (name "sigrok-cli")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/sigrok-cli/sigrok-cli-"
                    version ".tar.gz"))
              (sha256
               (base32
                "072ylscp0ppgii1k5j07hhv7dfmni4vyhxnsvxmgqgfyq9ldjsan"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("libsigrok" ,libsigrok)
       ("libsigrokdecode" ,libsigrokdecode)))
    (build-system gnu-build-system)
    (home-page "http://sigrok.org/wiki/Sigrok-cli")
    (synopsis "Command-line frontend for sigrok")
    (description "Sigrok-cli is a command-line frontend for sigrok.")
    (license license:gpl3+)))

(define-public pulseview
  (package
    (name "pulseview")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/pulseview/pulseview-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1f8f2342d5yam98mmcb8f9g2vslcwv486bmi4x45pxn68l82ky3q"))))
    (arguments
     `(#:configure-flags '("-DCMAKE_CXX_FLAGS=-fext-numeric-literals")))
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
    (build-system cmake-build-system)
    (home-page "http://www.sigrok.org/wiki/PulseView")
    (synopsis "Qt based logic analyzer, oscilloscope and MSO GUI for sigrok")
    (description "PulseView is a Qt based logic analyzer, oscilloscope and MSO GUI
for sigrok.")
    (license license:gpl3+)))
