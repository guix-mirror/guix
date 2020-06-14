;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Stefan <stefan-guix@vodafonemail.de>
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

(define-module (gnu bootloader grub)
  #:use-module (guix records)
  #:use-module ((guix utils) #:select (%current-system))
  #:use-module (guix gexp)
  #:use-module (gnu artwork)
  #:use-module (gnu bootloader)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu packages bootloaders)
  #:autoload   (gnu packages gtk) (guile-cairo guile-rsvg)
  #:autoload   (gnu packages xorg) (xkeyboard-config)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:export (grub-theme
            grub-theme?
            grub-theme-image
            grub-theme-resolution
            grub-theme-color-normal
            grub-theme-color-highlight
            grub-theme-gfxmode

            grub-bootloader
            grub-efi-bootloader
            grub-mkrescue-bootloader
            grub-minimal-bootloader

            grub-configuration))

;;; Commentary:
;;;
;;; Configuration of GNU GRUB.
;;;
;;; Code:

(define* (normalize-file file mount-point store-directory-prefix)
  "Strip MOUNT-POINT and prepend STORE-DIRECTORY-PREFIX, if any, to FILE, a
G-expression or other lowerable object denoting a file name."

  (define (strip-mount-point mount-point file)
    (if mount-point
        (if (string=? mount-point "/")
            file
            #~(let ((file #$file))
                (if (string-prefix? #$mount-point file)
                    (substring #$file #$(string-length mount-point))
                    file)))
        file))

  (define (prepend-store-directory-prefix store-directory-prefix file)
    (if store-directory-prefix
        #~(string-append #$store-directory-prefix #$file)
        file))

  (prepend-store-directory-prefix store-directory-prefix
                                  (strip-mount-point mount-point file)))



(define-record-type* <grub-theme>
  ;; Default theme contributed by Felipe López.
  grub-theme make-grub-theme
  grub-theme?
  (image           grub-theme-image
                   (default (file-append %artwork-repository
                                         "/grub/GuixSD-fully-black-4-3.svg")))
  (resolution      grub-theme-resolution
                   (default '(1024 . 768)))
  (color-normal    grub-theme-color-normal
                   (default '((fg . light-gray) (bg . black))))
  (color-highlight grub-theme-color-highlight
                   (default '((fg . yellow) (bg . black))))
  (gfxmode         grub-theme-gfxmode
                   (default '("auto"))))          ;list of string


;;;
;;; Background image & themes.
;;;

(define (bootloader-theme config)
  "Return user defined theme in CONFIG if defined or a default theme
otherwise."
  (or (bootloader-configuration-theme config) (grub-theme)))

(define* (image->png image #:key width height)
  "Build a PNG of HEIGHT x WIDTH from IMAGE if its file suffix is \".svg\".
Otherwise the picture in IMAGE is just copied."
  (computed-file "grub-image.png"
                 (with-imported-modules '((gnu build svg))
                   (with-extensions (list guile-rsvg guile-cairo)
                     #~(if (string-suffix? ".svg" #+image)
                           (begin
                             (use-modules (gnu build svg))
                             (svg->png #+image #$output
                                       #:width #$width
                                       #:height #$height))
                           (copy-file #+image #$output))))))

(define* (grub-background-image config)
  "Return the GRUB background image defined in CONFIG or #f if none was found.
If the suffix of the image file is \".svg\", then it is converted into a PNG
file with the resolution provided in CONFIG."
  (let* ((theme (bootloader-theme config))
         (image (grub-theme-image theme)))
    (and image
         (match (grub-theme-resolution theme)
           (((? number? width) . (? number? height))
            (image->png image #:width width #:height height))
           (_ #f)))))

(define* (eye-candy config store-device store-mount-point
                    #:key store-directory-prefix port)
  "Return a gexp that writes to PORT (a port-valued gexp) the 'grub.cfg' part
concerned with graphics mode, background images, colors, and all that.
STORE-DEVICE designates the device holding the store, and STORE-MOUNT-POINT is
its mount point; these are used to determine where the background image and
fonts must be searched for.  STORE-DIRECTORY-PREFIX is a directory prefix to
prepend to any store file name."
  (define (setup-gfxterm config font-file)
    (if (memq 'gfxterm (bootloader-configuration-terminal-outputs config))
        #~(format #f "
if loadfont ~a; then
  set gfxmode=~a
  insmod all_video
  insmod gfxterm
fi~%"
                  #+font-file
                  #$(string-join
                     (grub-theme-gfxmode (bootloader-theme config))
                     ";"))
        ""))

  (define (theme-colors type)
    (let* ((theme  (bootloader-theme config))
           (colors (type theme)))
      (string-append (symbol->string (assoc-ref colors 'fg)) "/"
                     (symbol->string (assoc-ref colors 'bg)))))

  (define font-file
    (normalize-file (file-append grub "/share/grub/unicode.pf2")
                    store-mount-point
                    store-directory-prefix))

  (define image
    (normalize-file (grub-background-image config)
                    store-mount-point
                    store-directory-prefix))

  (and image
       #~(format #$port "
# Set 'root' to the partition that contains /gnu/store.
~a

~a
~a

insmod png
if background_image ~a; then
  set color_normal=~a
  set color_highlight=~a
else
  set menu_color_normal=cyan/blue
  set menu_color_highlight=white/blue
fi~%"
                 #$(grub-root-search store-device font-file)
                 #$(setup-gfxterm config font-file)
                 #$(grub-setup-io config)

                 #$image
                 #$(theme-colors grub-theme-color-normal)
                 #$(theme-colors grub-theme-color-highlight))))


;;;
;;; Configuration file.
;;;

(define* (keyboard-layout-file layout
                               #:key
                               (grub grub))
  "Process the X keyboard layout description LAYOUT, a <keyboard-layout> record,
and return a file in the format for GRUB keymaps.  LAYOUT must be present in
the 'share/X11/xkb/symbols/' directory of 'xkeyboard-config'."
  (define builder
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))

          ;; 'grub-kbdcomp' passes all its arguments but '-o' to 'ckbcomp'
          ;; (from the 'console-setup' package).
          (invoke #+(file-append grub "/bin/grub-mklayout")
                  "-i" #+(keyboard-layout->console-keymap layout)
                  "-o" #$output))))

  (computed-file (string-append "grub-keymap."
                                (string-map (match-lambda
                                              (#\, #\-)
                                              (chr chr))
                                            (keyboard-layout-name layout)))
                 builder))

(define (grub-setup-io config)
  "Return GRUB commands to configure the input / output interfaces.  The result
is a string that can be inserted in grub.cfg."
  (let* ((symbols->string (lambda (list)
                           (string-join (map symbol->string list) " ")))
         (outputs (bootloader-configuration-terminal-outputs config))
         (inputs (bootloader-configuration-terminal-inputs config))
         (unit (bootloader-configuration-serial-unit config))
         (speed (bootloader-configuration-serial-speed config))

         ;; Respectively, GRUB_TERMINAL_OUTPUT and GRUB_TERMINAL_INPUT,
         ;; as documented in GRUB manual section "Simple Configuration
         ;; Handling".
         (valid-outputs '(console serial serial_0 serial_1 serial_2 serial_3
                          gfxterm vga_text mda_text morse spkmodem))
         (valid-inputs '(console serial serial_0 serial_1 serial_2 serial_3
                         at_keyboard usb_keyboard))

         (io (string-append
               "terminal_output "
               (symbols->string
                 (map
                   (lambda (output)
                     (if (memq output valid-outputs) output #f)) outputs)) "\n"
               (if (null? inputs)
                 ""
                 (string-append
                   "terminal_input "
                   (symbols->string
                     (map
                       (lambda (input)
                         (if (memq input valid-inputs) input #f)) inputs)) "\n"))
               ;; UNIT and SPEED are arguments to the same GRUB command
               ;; ("serial"), so we process them together.
               (if (or unit speed)
                 (string-append
                   "serial"
                   (if unit
                     ;; COM ports 1 through 4
                     (if (and (exact-integer? unit) (<= unit 3) (>= unit 0))
                       (string-append " --unit=" (number->string unit))
                       #f)
                     "")
                   (if speed
                     (if (exact-integer? speed)
                       (string-append " --speed=" (number->string speed))
                       #f)
                     ""))
                 ""))))
    (format #f "~a" io)))

(define (grub-root-search device file)
  "Return the GRUB 'search' command to look for DEVICE, which contains FILE,
a gexp.  The result is a gexp that can be inserted in the grub.cfg-generation
code."
  ;; Usually FILE is a file name gexp like "/gnu/store/…-linux/vmlinuz", but
  ;; it can also be something like "(hd0,msdos1)/vmlinuz" in the case of
  ;; custom menu entries.  In the latter case, don't emit a 'search' command.
  (if (and (string? file) (not (string-prefix? "/" file)))
      ""
      (match device
        ;; Preferably refer to DEVICE by its UUID or label.  This is more
        ;; efficient and less ambiguous, see <http://bugs.gnu.org/22281>.
        ((? uuid? uuid)
         (format #f "search --fs-uuid --set ~a"
                 (uuid->string device)))
        ((? file-system-label? label)
         (format #f "search --label --set ~a"
                 (file-system-label->string label)))
        ((or #f (? string?))
         #~(format #f "search --file --set ~a" #$file)))))

(define* (grub-configuration-file config entries
                                  #:key
                                  (system (%current-system))
                                  (old-entries '())
                                  store-directory-prefix)
  "Return the GRUB configuration file corresponding to CONFIG, a
<bootloader-configuration> object, and where the store is available at
STORE-FS, a <file-system> object.  OLD-ENTRIES is taken to be a list of menu
entries corresponding to old generations of the system.
STORE-DIRECTORY-PREFIX may be used to specify a store prefix, as is required
when booting a root file system on a Btrfs subvolume."
  (define all-entries
    (append entries (bootloader-configuration-menu-entries config)))
  (define (menu-entry->gexp entry)
    (let ((label (menu-entry-label entry))
          (linux (menu-entry-linux entry))
          (device (menu-entry-device entry))
          (device-mount-point (menu-entry-device-mount-point entry)))
      (if linux
          (let ((arguments (menu-entry-linux-arguments entry))
                (linux (normalize-file linux
                                       device-mount-point
                                       store-directory-prefix))
                (initrd (normalize-file (menu-entry-initrd entry)
                                        device-mount-point
                                        store-directory-prefix)))
         ;; Here DEVICE is the store and DEVICE-MOUNT-POINT is its mount point.
         ;; Use the right file names for LINUX and INITRD in case
         ;; DEVICE-MOUNT-POINT is not "/", meaning that the store is on a
         ;; separate partition.

         ;; When BTRFS-SUBVOLUME-FILE-NAME is defined, prepend it the linux and
         ;; initrd paths, to allow booting from a Btrfs subvolume.
         #~(format port "menuentry ~s {
  ~a
  linux ~a ~a
  initrd ~a
}~%"
                   #$label
                   #$(grub-root-search device linux)
                   #$linux (string-join (list #$@arguments))
                   #$initrd))
          (let ((kernel (menu-entry-multiboot-kernel entry))
                (arguments (menu-entry-multiboot-arguments entry))
                (modules (menu-entry-multiboot-modules entry))
                (root-index 1))            ; XXX EFI will need root-index 2
        #~(format port "
menuentry ~s {
  multiboot ~a root=device:hd0s~a~a~a
}~%"
                  #$label
                  #$kernel
                  #$root-index (string-join (list #$@arguments) " " 'prefix)
                  (string-join (map string-join '#$modules)
                               "\n  module " 'prefix))))))

  (define (sugar)
    (let* ((entry (first all-entries))
           (device (menu-entry-device entry))
           (mount-point (menu-entry-device-mount-point entry)))
      (eye-candy config
                 device
                 mount-point
                 #:store-directory-prefix store-directory-prefix
                 #:port #~port)))

  (define keyboard-layout-config
    (let* ((layout (bootloader-configuration-keyboard-layout config))
           (grub   (bootloader-package
                    (bootloader-configuration-bootloader config)))
           (keymap* (and layout
                         (keyboard-layout-file layout #:grub grub)))
           (keymap (and keymap*
                        (if store-directory-prefix
                            #~(string-append #$store-directory-prefix
                                             #$keymap*)
                            keymap*))))
      #~(when #$keymap
          (format port "\
insmod keylayouts
keymap ~a~%" #$keymap))))

  (define builder
    #~(call-with-output-file #$output
        (lambda (port)
          (format port
                  "# This file was generated from your Guix configuration.  Any changes
# will be lost upon reconfiguration.
")
          #$(sugar)
          #$keyboard-layout-config
          (format port "
set default=~a
set timeout=~a~%"
                  #$(bootloader-configuration-default-entry config)
                  #$(bootloader-configuration-timeout config))
          #$@(map menu-entry->gexp all-entries)

          #$@(if (pair? old-entries)
                 #~((format port "
submenu \"GNU system, old configurations...\" {~%")
                    #$@(map menu-entry->gexp old-entries)
                    (format port "}~%"))
                 #~())
          (format port "
if [ \"${grub_platform}\" == efi ]; then
  menuentry \"Firmware setup\" {
    fwsetup
  }
fi~%"))))

  ;; Since this file is rather unique, there's no point in trying to
  ;; substitute it.
  (computed-file "grub.cfg" builder
                 #:options '(#:local-build? #t
                             #:substitutable? #f)))



;;;
;;; Install procedures.
;;;

(define install-grub
  #~(lambda (bootloader device mount-point)
      (let ((grub (string-append bootloader "/sbin/grub-install"))
            (install-dir (string-append mount-point "/boot")))
        ;; Install GRUB on DEVICE which is mounted at MOUNT-POINT. If DEVICE
        ;; is #f, then we populate the disk-image rooted at MOUNT-POINT.
        (if device
            (begin
              ;; Tell 'grub-install' that there might be a LUKS-encrypted
              ;; /boot or root partition.
              (setenv "GRUB_ENABLE_CRYPTODISK" "y")

              ;; Hide potentially confusing messages from the user, such as
              ;; "Installing for i386-pc platform."
              (invoke/quiet grub "--no-floppy" "--target=i386-pc"
                            "--boot-directory" install-dir
                            device))
            ;; When creating a disk-image, only install GRUB modules.
            (copy-recursively (string-append bootloader "/lib/")
                              install-dir)))))

(define install-grub-disk-image
  #~(lambda (bootloader root-index image)
      ;; Install GRUB on the given IMAGE. The root partition index is
      ;; ROOT-INDEX.
      (let ((grub-mkimage
             (string-append bootloader "/bin/grub-mkimage"))
            (modules '("biosdisk" "part_msdos" "fat" "ext2"))
            (grub-bios-setup
             (string-append bootloader "/sbin/grub-bios-setup"))
            (root-device (format #f "hd0,msdos~a" root-index))
            (boot-img (string-append bootloader "/lib/grub/i386-pc/boot.img"))
            (device-map "device.map"))

        ;; Create a minimal, standalone GRUB image that will be written
        ;; directly in the MBR-GAP (space between the end of the MBR and the
        ;; first partition).
        (apply invoke grub-mkimage
               "-O" "i386-pc"
               "-o" "core.img"
               "-p" (format #f "(~a)/boot/grub" root-device)
               modules)

        ;; Create a device mapping file.
        (call-with-output-file device-map
          (lambda (port)
            (format port "(hd0) ~a~%" image)))

        ;; Copy the default boot.img, that will be written on the MBR sector
        ;; by GRUB-BIOS-SETUP.
        (copy-file boot-img "boot.img")

        ;; Install both the "boot.img" and the "core.img" files on the given
        ;; IMAGE. On boot, the MBR sector will execute the minimal GRUB
        ;; written in the MBR-GAP. GRUB configuration and missing modules will
        ;; be read from ROOT-DEVICE.
        (invoke grub-bios-setup
                "-m" device-map
                "-r" root-device
                "-d" "."
                image))))

(define install-grub-efi
  #~(lambda (bootloader efi-dir mount-point)
      ;; Install GRUB onto the EFI partition mounted at EFI-DIR, for the
      ;; system whose root is mounted at MOUNT-POINT.
      (let ((grub-install (string-append bootloader "/sbin/grub-install"))
            (install-dir (string-append mount-point "/boot"))
            ;; When installing Guix, it's common to mount EFI-DIR below
            ;; MOUNT-POINT rather than /boot/efi on the live image.
            (target-esp (if (file-exists? (string-append mount-point efi-dir))
                            (string-append mount-point efi-dir)
                            efi-dir)))
        ;; Tell 'grub-install' that there might be a LUKS-encrypted /boot or
        ;; root partition.
        (setenv "GRUB_ENABLE_CRYPTODISK" "y")
        (invoke/quiet grub-install "--boot-directory" install-dir
                      "--bootloader-id=Guix"
                      "--efi-directory" target-esp))))



;;;
;;; Bootloader definitions.
;;;

(define grub-bootloader
  (bootloader
   (name 'grub)
   (package grub)
   (installer install-grub)
   (disk-image-installer install-grub-disk-image)
   (configuration-file "/boot/grub/grub.cfg")
   (configuration-file-generator grub-configuration-file)))

(define* grub-minimal-bootloader
  (bootloader
   (inherit grub-bootloader)
   (package grub-minimal)))

(define* grub-efi-bootloader
  (bootloader
   (inherit grub-bootloader)
   (installer install-grub-efi)
   (disk-image-installer #f)
   (name 'grub-efi)
   (package grub-efi)))

(define* grub-mkrescue-bootloader
  (bootloader
   (inherit grub-efi-bootloader)
   (package grub-hybrid)))


;;;
;;; Compatibility macros.
;;;

(define-syntax grub-configuration
  (syntax-rules (grub)
                ((_ (grub package) fields ...)
                 (if (eq? package grub)
                     (bootloader-configuration
                      (bootloader grub-bootloader)
                      fields ...)
                   (bootloader-configuration
                    (bootloader grub-efi-bootloader)
                    fields ...)))
                ((_ fields ...)
                 (bootloader-configuration
                  (bootloader grub-bootloader)
                  fields ...))))

;;; grub.scm ends here
