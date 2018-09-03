;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages hardware)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;; This is a module for packages related to physical hardware that don't (yet)
;; have a more specific home like gps.scm, security-token.scm, &c.

(define-public ddcutil
  (package
    (name "ddcutil")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.ddcutil.com/tarballs/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "1b4bm3zhk5vnad6fxf0mn8nrlj3fngifl7nzxgxw0n56hlv7ccv0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("eudev" ,eudev)
       ("glib" ,glib)
       ("libdrm" ,libdrm)               ; enhanced diagnostics
       ("libusb" ,libusb)               ; support USB monitors
       ("libx11" ,libx11)               ; enhanced diagnostics
       ("libxrandr" ,libxrandr)
       ("zlib" ,zlib)))
    (home-page "https://www.ddcutil.com/")
    (synopsis "Control external monitor settings")
    (description
     "ddcutil can query and modify most external monitors' settings, such as
brightness, colour levels, and input sources.  Generally speaking, any setting
that can be changed by pressing buttons on the monitor can be modified by
ddcutil.

ddcutil communicates directly with monitors implementing the Monitor Control
Command Set (@dfn{MCCS}).  It usually does so through the the Display Data
Channel Command Interface (@dfn{DDC/CI}) protocol on the I2C bus, but can also
communicate over USB as per the USB Monitor Control Class Specification.

One particular use case is in colour profile management.  Monitor calibration
is relative to the monitor colour settings currently in effect, e.g. red gain.
ddcutil allows colour-related settings to be saved at the time a monitor is
calibrated, and restored when the calibration is applied.")
    (license (list license:bsd-3        ; FindDDCUtil.cmake
                   license:gpl2+))))    ; everything else

(define-public msr-tools
  (package
    (name "msr-tools")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://01.org/sites/default/files/downloads/"
                           name "/" name "-" version ".zip"))
       (sha256
        (base32 "07hxmddg0l31kjfmaq84ni142lbbvgq6391r8bd79wpm819pnigr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "sbindir=" (assoc-ref %outputs "out") "/sbin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'install 'create-output-directory
           (lambda* (#:key outputs #:allow-other-keys)
             ;; 'make install' assumes that sbindir exists.
             (let* ((out  (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin")))
               (mkdir-p sbin)
               #t))))
       #:tests? #f))                    ; no test suite
    (native-inputs
     `(("unzip" ,unzip)))
    ;; These registers and the CPUID instruction only exist on (most) x86 chips.
    (supported-systems (list "i686-linux" "x86_64-linux"))
    (home-page "https://01.org/msr-tools/")
    (synopsis "Read and write Model-Specific Registers (@dfn{MSR})")
    (description
     "The MSR Tools project provides console utilities to directly access the
Model-Specific Registers (@dfn{MSR}s) and CPU ID of Intel-compatible processors:

@itemize
@item @command{cpuid}: show identification and feature information of any CPU
@item @command{rdmsr}: read MSRs from any CPU or all CPUs
@item @command{wrmsr}: write to MSRs on any CPU or all CPUs
@end itemize

These tools can be used to query and modify certain low-level CPU parameters,
such as the Turbo Boost ratio and Thermal Design Power (@dfn{TDP}) limits.

MSR addresses differ (greatly) between processors, and any such modification can
be dangerous and may void your CPU or system board's warranty.")
    (license license:gpl2)))     ; cpuid.c is gpl2, {rd,wr}msr.c are gpl2+
