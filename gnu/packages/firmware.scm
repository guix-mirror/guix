;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages perl))

(define-public ath9k-htc-firmware
  (package
    (name "ath9k-htc-firmware")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/qca/open-ath9k-htc-firmware.git")
                    (commit version)))
              (sha256
               (base32
                "0dgqfp4cbky79vzjrdy3j462l8figymzrk2v0jalmmz3lkxw88ww"))
              (patches (list (search-patch "ath9k-htc-firmware-objcopy.patch")))))
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
    (native-inputs `(("cross-gcc" ,xgcc-xtensa)
                     ("cross-binutils" ,(cross-binutils "xtensa-elf"))
                     ("cmake" ,cmake)
                     ("perl" ,perl)))
    (home-page "http://wireless.kernel.org/en/users/Drivers/ath9k_htc")
    (synopsis "Firmware for the Atheros AR7010 and AR9271 USB 802.11n NICs")
    (description
     "This is the firmware for the Qualcomm Atheros AR7010 and AR9271 USB
802.11n NICs (aka. Wi-Fi USB dongles.)  It is used by the ath9k driver of
Linux-libre.")
    (license (bsd-style "http://directory.fsf.org/wiki/License:ClearBSD"))))
