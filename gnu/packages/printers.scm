;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages printers)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt))

;; This is a module for packages related to printer-like devices, but not
;; related to CUPS.

(define-public robocut
  (package
    (name "robocut")
    (version "1.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Timmmm/robocut.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dp9cssyik63yvkk35s51v94a873x751iqg93qzd8dpqkmz5z8gn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "Robocut.pro"
                          (("/usr/")
                           (string-append out "/")))

                        (invoke "qmake"
                                (string-append "PREFIX=" out))
                        #t))))))
    (inputs
     `(("libusb" ,libusb)
       ("qt" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qmake" ,qtbase)))
    (synopsis "Graphical program to drive plotting cutters")
    (description
     "Robocut is a simple graphical program that allows you to cut graphics
with Graphtec and Sihouette plotting cutters using an SVG file as its input.")
    (home-page "http://robocut.org")
    (license license:gpl3+)))

(define-public brlaser
  (let ((commit "779f71e80fcc583f4537c844de141cf8e6a738fe")
        (revision "1"))
    (package
      (name "brlaser")
      (version (git-version "4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pdewacht/brlaser.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0yrz9706660hdfi2y0ndp254r0vw9y665hwmxbbzfnz4ai13vj50"))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags
         (list (string-append "-DCUPS_DATA_DIR="
                              (assoc-ref %outputs "out")
                              "/share/cups")
               (string-append "-DCUPS_SERVER_BIN="
                              (assoc-ref %outputs "out")
                              "/lib/cups"))))
      (inputs
       `(("ghostscript" ,ghostscript)
         ("cups" ,cups)
         ("zlib" ,zlib)))
      (home-page "https://github.com/pdewacht/brlaser")
      (synopsis "Brother laser printer driver")
      (description "Brlaser is a CUPS driver for Brother laser printers.  This
driver is known to work with these printers:

@enumerate
@item Brother DCP-1510
@item Brother DCP-7030
@item Brother DCP-7040
@item Brother DCP-7055
@item Brother DCP-7055W
@item Brother DCP-7065DN
@item Brother DCP-7080
@item Brother HL-1200 series
@item Brother HL-L2320D series
@item Brother HL-L2360D series
@item Brother MFC-7240
@item Brother MFC-7360N
@end enumerate

It partially works with printers from the Brother HL-L2300D series.")
      (license license:gpl2+))))
