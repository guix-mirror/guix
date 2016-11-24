;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages firmware)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages perl))

(define-public ath9k-htc-firmware
  (package
    (name "ath9k-htc-firmware")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/qca/open-ath9k-htc-firmware.git")
                    (commit version)))
              (sha256
               (base32
                "16jbj8avg5jkgvq5lxm0hdxxn4c3zn7fx8b4nxllvr024apk9w23"))
              (file-name (string-append name "-" version "-checkout"))
              (patches (search-patches "ath9k-htc-firmware-objcopy.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'pre-configure
                 (lambda* (#:key inputs #:allow-other-keys)
                   (chdir "target_firmware")

                   ;; 'configure' is a simple script that runs 'cmake' with
                   ;; the right flags.
                   (substitute* "configure"
                     (("^TOOLCHAIN=.*$")
                      (string-append "TOOLCHAIN="
                                     (assoc-ref inputs "cross-gcc")
                                     "\n"))))
                 (alist-replace
                  'install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out    (assoc-ref outputs "out"))
                           (fw-dir (string-append out "/lib/firmware")))
                      (mkdir-p fw-dir)
                      (for-each (lambda (file)
                                  (copy-file file
                                             (string-append fw-dir "/"
                                                            (basename file))))
                                (find-files "." "\\.fw$"))
                      #t))
                  %standard-phases))
       #:tests? #f))

    ;; The firmware is cross-compiled using a "bare bones" compiler (no libc.)
    ;; Use our own tool chain for that.
    (native-inputs `(("cross-gcc" ,(cross-gcc "xtensa-elf"))
                     ("cross-binutils" ,(cross-binutils "xtensa-elf"))
                     ("cmake" ,cmake)
                     ("perl" ,perl)))
    (home-page "http://wireless.kernel.org/en/users/Drivers/ath9k_htc")
    (synopsis "Firmware for the Atheros AR7010 and AR9271 USB 802.11n NICs")
    (description
     "This is the firmware for the Qualcomm Atheros AR7010 and AR9271 USB
802.11n NICs (aka Wi-Fi USB dongles).  It is used by the ath9k driver of
Linux-libre.")
    (license (non-copyleft "http://directory.fsf.org/wiki/License:ClearBSD"))))

(define-public b43-tools
  (let ((commit "8dce53297966b31b6c70a7a03c2433978dd9f288")
        (rev "1"))
    (package
      (name "b43-tools")
      (version (string-append "20140625-" rev "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.bues.ch/git/b43-tools.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "08k7sdr9jagm43r2zv4h03j86klhkblpk73p12444a3vzg1gy1lv"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("flex" ,flex)
         ("bison" ,bison)))
      (arguments
       `(#:modules ((srfi srfi-1)
                    (guix build gnu-build-system)
                    (guix build utils))
         #:tests? #f                    ;no tests
         #:phases
         (let ((subdirs '("assembler" "disassembler")))
           (modify-phases %standard-phases
             (delete 'configure)
             (add-before 'build 'patch-/bin/true
               (lambda _
                 (substitute* (find-files "." "Makefile")
                   (("/bin/true") ":"))
                 #t))
             (replace 'build
               (lambda _
                 (every (lambda (dir)
                          (zero? (system* "make" "-C" dir "CC=gcc")))
                        subdirs)))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (mkdir-p (string-append out "/bin"))
                   (every (lambda (dir)
                            (zero?
                             (system* "make" "-C" dir
                                      (string-append "PREFIX=" out)
                                      "install")))
                          subdirs))))))))
      (home-page
       "http://bues.ch/cms/hacking/misc.html#linux_b43_driver_firmware_tools")
      (synopsis "Collection of tools for the b43 wireless driver")
      (description
       "The b43 firmware tools is a collection of firmware extractor,
assembler, disassembler, and debugging tools for the Linux kernel b43 wireless
driver.")
      (license gpl2))))

(define-public openfwwf-firmware
  (package
    (name "openfwwf-firmware")
    (version "5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://netweb.ing.unibs.it/~openfwwf/firmware/"
                           "openfwwf-" version ".tar.gz"))
       (sha256
        (base32
         "1p60gdi7w88s7qw82d3g9v7mk887mhvidf4l5q5hh09j10h37q4x"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("b43-tools" ,b43-tools)))
    (arguments
     `(#:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")
                                         "/lib/firmware/b43-open"))
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "http://netweb.ing.unibs.it/~openfwwf/")
    (synopsis "Firmware for BCM43xx devices")
    (description
     "This is firmware from Open FirmWare for WiFi networks (OpenFWWF) for the
Broadcom/AirForce chipset BCM43xx with Wireless-Core Revision 5.  It is used
by the b43-open driver of Linux-libre.")
    (license gpl2)))
