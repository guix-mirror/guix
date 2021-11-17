;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build bootloader)
  #:use-module (guix build utils)
  #:use-module (guix utils)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (rnrs io ports)
  #:use-module (rnrs io simple)
  #:export (write-file-on-device
            install-efi-loader))


;;;
;;; Writing utils.
;;;

(define (write-file-on-device file size device offset)
  "Write SIZE bytes from FILE to DEVICE starting at OFFSET."
  (call-with-input-file file
    (lambda (input)
      (let ((bv (get-bytevector-n input size)))
        (call-with-port
         ;; Do not use "call-with-output-file" that would truncate the file.
         (open-file-output-port device
                                (file-options no-truncate no-fail)
                                (buffer-mode block)
                                ;; Use the binary-friendly ISO-8859-1
                                ;; encoding.
                                (make-transcoder (latin-1-codec)))
         (lambda (output)
           (seek output offset SEEK_SET)
           (put-bytevector output bv)))))))


;;;
;;; EFI bootloader.
;;;

(define (install-efi grub grub-config esp)
  "Write a self-contained GRUB EFI loader to the mounted ESP using GRUB-CONFIG."
  (let* ((system %host-type)
         ;; Hard code the output location to a well-known path recognized by
         ;; compliant firmware. See "3.5.1.1 Removable Media Boot Behaviour":
         ;; http://www.uefi.org/sites/default/files/resources/UEFI%20Spec%202_6.pdf
         (grub-mkstandalone (string-append grub "/bin/grub-mkstandalone"))
         (efi-directory (string-append esp "/EFI/BOOT"))
         ;; Map grub target names to boot file names.
         (efi-targets (cond ((string-prefix? "x86_64" system)
                             '("x86_64-efi" . "BOOTX64.EFI"))
                            ((string-prefix? "i686" system)
                             '("i386-efi" . "BOOTIA32.EFI"))
                            ((string-prefix? "armhf" system)
                             '("arm-efi" . "BOOTARM.EFI"))
                            ((string-prefix? "aarch64" system)
                             '("arm64-efi" . "BOOTAA64.EFI")))))
    ;; grub-mkstandalone requires a TMPDIR to prepare the firmware image.
    (setenv "TMPDIR" esp)

    (mkdir-p efi-directory)
    (invoke grub-mkstandalone "-O" (car efi-targets)
            "-o" (string-append efi-directory "/"
                                (cdr efi-targets))
            ;; Graft the configuration file onto the image.
            (string-append "boot/grub/grub.cfg=" grub-config))))

(define (install-efi-loader grub-efi esp)
  "Install in ESP directory the given GRUB-EFI bootloader.  Configure it to
load the Grub bootloader located in the 'Guix_image' root partition."
  (let ((grub-config "grub.cfg"))
    (call-with-output-file grub-config
      (lambda (port)
        ;; Create a tiny configuration file telling the embedded grub where to
        ;; load the real thing.  XXX This is quite fragile, and can prevent
        ;; the image from booting when there's more than one volume with this
        ;; label present.  Reproducible almost-UUIDs could reduce the risk
        ;; (not eliminate it).
        (format port
                "insmod part_msdos~@
               insmod part_gpt~@
               search --set=root --label Guix_image~@
               configfile /boot/grub/grub.cfg~%")))
    (install-efi grub-efi grub-config esp)
    (delete-file grub-config)))
