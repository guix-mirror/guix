;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:autoload   (gnu packages bootloaders) (grub)
  #:autoload   (gnu packages gtk) (guile-cairo guile-rsvg)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (grub-image
            grub-image?
            grub-image-aspect-ratio
            grub-image-file

            grub-theme
            grub-theme?
            grub-theme-images
            grub-theme-color-normal
            grub-theme-color-highlight

            %background-image
            %default-theme

            grub-bootloader
            grub-efi-bootloader
            grub-mkrescue-bootloader

            grub-configuration))

;;; Commentary:
;;;
;;; Configuration of GNU GRUB.
;;;
;;; Code:

(define (strip-mount-point mount-point file)
  "Strip MOUNT-POINT from FILE, which is a gexp or other lowerable object
denoting a file name."
  (match mount-point
    ((? string? mount-point)
     (if (string=? mount-point "/")
         file
         #~(let ((file #$file))
             (if (string-prefix? #$mount-point file)
                 (substring #$file #$(string-length mount-point))
                 file))))
    (#f file)))

(define-record-type* <grub-image>
  grub-image make-grub-image
  grub-image?
  (aspect-ratio    grub-image-aspect-ratio        ;rational number
                   (default 4/3))
  (file            grub-image-file))              ;file-valued gexp (SVG)

(define-record-type* <grub-theme>
  grub-theme make-grub-theme
  grub-theme?
  (images          grub-theme-images
                   (default '()))                 ;list of <grub-image>
  (color-normal    grub-theme-color-normal
                   (default '((fg . cyan) (bg . blue))))
  (color-highlight grub-theme-color-highlight
                   (default '((fg . white) (bg . blue)))))

(define %background-image
  (grub-image
   (aspect-ratio 4/3)
   (file (file-append %artwork-repository
                      "/grub/GuixSD-fully-black-4-3.svg"))))

(define %default-theme
  ;; Default theme contributed by Felipe López.
  (grub-theme
   (images (list %background-image))
   (color-highlight '((fg . yellow) (bg . black)))
   (color-normal    '((fg . light-gray) (bg . black))))) ;XXX: #x303030


;;;
;;; Background image & themes.
;;;

(define (bootloader-theme config)
  "Return user defined theme in CONFIG if defined or %default-theme
otherwise."
  (or (bootloader-configuration-theme config) %default-theme))

(define* (svg->png svg #:key width height)
  "Build a PNG of HEIGHT x WIDTH from SVG."
  (computed-file "grub-image.png"
                 (with-imported-modules '((gnu build svg))
                   (with-extensions (list guile-rsvg guile-cairo)
                     #~(begin
                         (use-modules (gnu build svg))
                         (svg->png #+svg #$output
                                   #:width #$width
                                   #:height #$height))))))

(define* (grub-background-image config #:key (width 1024) (height 768))
  "Return the GRUB background image defined in CONFIG with a ratio of
WIDTH/HEIGHT, or #f if none was found."
  (let* ((ratio (/ width height))
         (image (find (lambda (image)
                        (= (grub-image-aspect-ratio image) ratio))
                      (grub-theme-images
                       (bootloader-theme config)))))
    (and image
         (svg->png (grub-image-file image)
                   #:width width #:height height))))

(define* (eye-candy config store-device store-mount-point
                    #:key system port)
  "Return a gexp that writes to PORT (a port-valued gexp) the
'grub.cfg' part concerned with graphics mode, background images, colors, and
all that.  STORE-DEVICE designates the device holding the store, and
STORE-MOUNT-POINT is its mount point; these are used to determine where the
background image and fonts must be searched for.  SYSTEM must be the target
system string---e.g., \"x86_64-linux\"."
  (define setup-gfxterm-body
    ;; Intel and EFI systems need to be switched into graphics mode, whereas
    ;; most other modern architectures have no other mode and therefore don't
    ;; need to be switched.
    (if (string-match "^(x86_64|i[3-6]86)-" system)
        "
  # Leave 'gfxmode' to 'auto'.
  insmod video_bochs
  insmod video_cirrus
  insmod gfxterm

  if [ \"${grub_platform}\" == efi ]; then
    # This is for (U)EFI systems (these modules are unavailable in the
    # non-EFI GRUB.)  If we don't load them, GRUB boots in \"blind mode\",
    # which isn't convenient.
    insmod efi_gop
    insmod efi_uga
  else
    # These are specific to non-EFI Intel machines.
    insmod vbe
    insmod vga
  fi
"
        ""))

  (define (setup-gfxterm config font-file)
    (if (memq 'gfxterm (bootloader-configuration-terminal-outputs config))
        #~(format #f "if loadfont ~a; then
  setup_gfxterm
fi~%" #$font-file)
        ""))

  (define (theme-colors type)
    (let* ((theme  (bootloader-theme config))
           (colors (type theme)))
      (string-append (symbol->string (assoc-ref colors 'fg)) "/"
                     (symbol->string (assoc-ref colors 'bg)))))

  (define font-file
    (strip-mount-point store-mount-point
                       (file-append grub "/share/grub/unicode.pf2")))

  (define image
    (grub-background-image config))

  (and image
       #~(format #$port "
function setup_gfxterm {~a}

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
                 #$setup-gfxterm-body
                 #$(grub-root-search store-device font-file)
                 #$(setup-gfxterm config font-file)
                 #$(grub-setup-io config)

                 #$(strip-mount-point store-mount-point image)
                 #$(theme-colors grub-theme-color-normal)
                 #$(theme-colors grub-theme-color-highlight))))


;;;
;;; Configuration file.
;;;

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
                                  (old-entries '()))
  "Return the GRUB configuration file corresponding to CONFIG, a
<bootloader-configuration> object, and where the store is available at
STORE-FS, a <file-system> object.  OLD-ENTRIES is taken to be a list of menu
entries corresponding to old generations of the system."
  (define all-entries
    (append entries (bootloader-configuration-menu-entries config)))
  (define (menu-entry->gexp entry)
    (let ((device (menu-entry-device entry))
          (device-mount-point (menu-entry-device-mount-point entry))
          (label (menu-entry-label entry))
          (kernel (menu-entry-linux entry))
          (arguments (menu-entry-linux-arguments entry))
          (initrd (menu-entry-initrd entry)))
      ;; Here DEVICE is the store and DEVICE-MOUNT-POINT is its mount point.
      ;; Use the right file names for KERNEL and INITRD in case
      ;; DEVICE-MOUNT-POINT is not "/", meaning that the store is on a
      ;; separate partition.
      (let ((kernel  (strip-mount-point device-mount-point kernel))
            (initrd  (strip-mount-point device-mount-point initrd)))
        #~(format port "menuentry ~s {
  ~a
  linux ~a ~a
  initrd ~a
}~%"
                  #$label
                  #$(grub-root-search device kernel)
                  #$kernel (string-join (list #$@arguments))
                  #$initrd))))
  (define sugar
    (eye-candy config
               (menu-entry-device (first all-entries))
               (menu-entry-device-mount-point (first all-entries))
               #:system system
               #:port #~port))

  (define builder
    #~(call-with-output-file #$output
        (lambda (port)
          (format port
                  "# This file was generated from your Guix configuration.  Any changes
# will be lost upon reconfiguration.
")
          #$sugar
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
                 #~()))))

  (computed-file "grub.cfg" builder))



;;;
;;; Install procedures.
;;;

(define install-grub
  #~(lambda (bootloader device mount-point)
      ;; Install GRUB on DEVICE which is mounted at MOUNT-POINT.
      (let ((grub (string-append bootloader "/sbin/grub-install"))
            (install-dir (string-append mount-point "/boot")))
        ;; Tell 'grub-install' that there might be a LUKS-encrypted /boot or
        ;; root partition.
        (setenv "GRUB_ENABLE_CRYPTODISK" "y")

        (unless (zero? (system* grub "--no-floppy" "--target=i386-pc"
                                "--boot-directory" install-dir
                                device))
          (error "failed to install GRUB (BIOS)")))))

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
        (unless (zero? (system* grub-install "--boot-directory" install-dir
                                "--bootloader-id=Guix"
                                "--efi-directory" target-esp))
          (error "failed to install GRUB (EFI)")))))



;;;
;;; Bootloader definitions.
;;;

(define grub-bootloader
  (bootloader
   (name 'grub)
   (package grub)
   (installer install-grub)
   (configuration-file "/boot/grub/grub.cfg")
   (configuration-file-generator grub-configuration-file)))

(define* grub-efi-bootloader
  (bootloader
   (inherit grub-bootloader)
   (installer install-grub-efi)
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
