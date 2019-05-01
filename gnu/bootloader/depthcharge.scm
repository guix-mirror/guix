;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
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

(define-module (gnu bootloader depthcharge)
  #:use-module (gnu bootloader extlinux)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages bootloaders)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:export (depthcharge-bootloader))

(define (signed-kernel kernel kernel-arguments initrd)
  (define builder
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 binary-ports)
                       (rnrs bytevectors))
          (set-path-environment-variable "PATH" '("bin") (list #$dtc))

          ;; TODO: These files have to be writable, so we copy them.
          ;; This can probably be fixed by using a ".its" file, just
          ;; be careful not to break initrd loading.
          (copy-file #$kernel "zImage")
          (chmod "zImage" #o755)
          (copy-file (string-append (dirname #$kernel) "/lib/dtbs/"
                                    "rk3288-veyron-speedy.dtb")
                     "rk3288-veyron-speedy.dtb")
          (chmod "rk3288-veyron-speedy.dtb" #o644)
          (copy-file #$initrd "initrd")
          (chmod "initrd" #o644)

          (invoke (string-append #$u-boot-tools "/bin/mkimage")
                  "-D" "-I dts -O dtb -p 2048"
		  "-f" "auto"
                  "-A" "arm"
                  "-O" "linux"
                  "-T" "kernel"
                  "-C" "None"
                  "-d" "zImage"
                  "-a" "0"
                  "-b" "rk3288-veyron-speedy.dtb"
                  "-i" "initrd"
	          "image.itb")
          (call-with-output-file "bootloader.bin"
            (lambda (port)
              (put-bytevector port (make-bytevector 512 0))))
          (with-output-to-file "kernel-arguments"
	    (lambda ()
	      (display (string-join (list #$@kernel-arguments)))))
          (invoke (string-append #$vboot-utils "/bin/vbutil_kernel")
                  "--pack" #$output
                  "--version" "1"
                  "--vmlinuz" "image.itb"
		  "--arch" "arm"
		  "--keyblock" (string-append #$vboot-utils
                                              "/share/vboot-utils/devkeys/"
                                              "kernel.keyblock")
		  "--signprivate" (string-append #$vboot-utils
                                                 "/share/vboot-utils/devkeys/"
                                                 "kernel_data_key.vbprivk")
                  "--config" "kernel-arguments"
                  "--bootloader" "bootloader.bin"))))
  (computed-file "vmlinux.kpart" builder))

(define* (depthcharge-configuration-file config entries
                                         #:key
                                         (system (%current-system))
                                         (old-entries '()))
  (match entries
    ((entry)
     (let ((kernel (menu-entry-linux entry))
           (kernel-arguments (menu-entry-linux-arguments entry))
           (initrd (menu-entry-initrd entry)))
       ;; XXX: Make this a symlink.
       (signed-kernel kernel kernel-arguments initrd)))
    (_ (error "Too many bootloader menu entries!"))))

(define install-depthcharge
  #~(lambda (bootloader device mount-point)
      (let ((kpart (string-append mount-point
                                  "/boot/depthcharge/vmlinux.kpart")))
        (write-file-on-device kpart (stat:size (stat kpart)) device 0))))

(define depthcharge-bootloader
  (bootloader
   (name 'depthcharge)
   (package #f)
   (installer install-depthcharge)
   (configuration-file "/boot/depthcharge/vmlinux.kpart")
   (configuration-file-generator depthcharge-configuration-file)))
