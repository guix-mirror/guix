;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
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

(define-module (gnu system grub)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix records)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (gnu artwork)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:autoload   (gnu packages bootloaders) (grub)
  #:autoload   (gnu packages compression) (gzip)
  #:autoload   (gnu packages gtk) (guile-cairo guile-rsvg)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
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

            grub-configuration
            grub-configuration?
            grub-configuration-device
            grub-configuration-grub

            menu-entry
            menu-entry?

            grub-configuration-file))

;;; Commentary:
;;;
;;; Configuration of GNU GRUB.
;;;
;;; Code:

(define (strip-mount-point mount-point file)
  "Strip MOUNT-POINT from FILE, which is a gexp or other lowerable object
denoting a file name."
  (if (string=? mount-point "/")
      file
      #~(let ((file #$file))
          (if (string-prefix? #$mount-point file)
              (substring #$file #$(string-length mount-point))
              file))))

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

(define-record-type* <grub-configuration>
  grub-configuration make-grub-configuration
  grub-configuration?
  (grub            grub-configuration-grub           ; package
                   (default (@ (gnu packages bootloaders) grub)))
  (device          grub-configuration-device)        ; string
  (menu-entries    grub-configuration-menu-entries   ; list
                   (default '()))
  (default-entry   grub-configuration-default-entry  ; integer
                   (default 0))
  (timeout         grub-configuration-timeout        ; integer
                   (default 5))
  (theme           grub-configuration-theme          ; <grub-theme>
                   (default %default-theme)))

(define-record-type* <menu-entry>
  menu-entry make-menu-entry
  menu-entry?
  (label           menu-entry-label)
  (device          menu-entry-device       ; file system uuid, label, or #f
                   (default #f))
  (device-mount-point menu-entry-device-mount-point
                      (default "/"))
  (linux           menu-entry-linux)
  (linux-arguments menu-entry-linux-arguments
                   (default '()))          ; list of string-valued gexps
  (initrd          menu-entry-initrd))     ; file name of the initrd as a gexp


;;;
;;; Background image & themes.
;;;

(define* (svg->png svg #:key width height)
  "Build a PNG of HEIGHT x WIDTH from SVG."
  (gexp->derivation "grub-image.png"
                    (with-imported-modules '((gnu build svg))
                      #~(begin
                          ;; We need these two libraries.
                          (add-to-load-path (string-append #$guile-rsvg
                                                           "/share/guile/site/"
                                                           (effective-version)))
                          (add-to-load-path (string-append #$guile-cairo
                                                           "/share/guile/site/"
                                                           (effective-version)))

                          (use-modules (gnu build svg))
                          (svg->png #$svg #$output
                                    #:width #$width
                                    #:height #$height)))))

(define* (grub-background-image config #:key (width 1024) (height 768))
  "Return the GRUB background image defined in CONFIG with a ratio of
WIDTH/HEIGHT, or #f if none was found."
  (let* ((ratio (/ width height))
         (image (find (lambda (image)
                        (= (grub-image-aspect-ratio image) ratio))
                      (grub-theme-images (grub-configuration-theme config)))))
    (if image
        (svg->png (grub-image-file image)
                  #:width width #:height height)
        (with-monad %store-monad
          (return #f)))))

(define* (eye-candy config store-device store-mount-point
                    #:key system port)
  "Return in %STORE-MONAD a gexp that writes to PORT (a port-valued gexp) the
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

  terminal_output gfxterm
"
        ""))

  (define (theme-colors type)
    (let* ((theme  (grub-configuration-theme config))
           (colors (type theme)))
      (string-append (symbol->string (assoc-ref colors 'fg)) "/"
                     (symbol->string (assoc-ref colors 'bg)))))

  (define font-file
    (strip-mount-point store-mount-point
                       (file-append grub "/share/grub/unicode.pf2")))

  (mlet* %store-monad ((image (grub-background-image config)))
    (return (and image
                 #~(format #$port "
function setup_gfxterm {~a}

# Set 'root' to the partition that contains /gnu/store.
~a

if loadfont ~a; then
  setup_gfxterm
fi

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
                           #$font-file

                           #$(strip-mount-point store-mount-point image)
                           #$(theme-colors grub-theme-color-normal)
                           #$(theme-colors grub-theme-color-highlight))))))


;;;
;;; Configuration file.
;;;

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
        ((? bytevector? uuid)
         (format #f "search --fs-uuid --set ~a"
                 (uuid->string device)))
        ((? string? label)
         (format #f "search --label --set ~a" label))
        (#f
         #~(format #f "search --file --set ~a" #$file)))))

(define (boot-parameters->menu-entry conf)
  "Convert a <boot-parameters> instance to a corresponding <menu-entry>."
  (menu-entry
   (label (boot-parameters-label conf))
   (device (boot-parameters-store-device conf))
   (device-mount-point (boot-parameters-store-mount-point conf))
   (linux (boot-parameters-kernel conf))
   (linux-arguments (boot-parameters-kernel-arguments conf))
   (initrd (boot-parameters-initrd conf))))

(define* (grub-configuration-file config entries
                                  #:key
                                  (system (%current-system))
                                  (old-entries '()))
  "Return the GRUB configuration file corresponding to CONFIG, a
<grub-configuration> object, and where the store is available at STORE-FS, a
<file-system> object.  OLD-ENTRIES is taken to be a list of menu entries
corresponding to old generations of the system."
  (define all-entries
    (append (map boot-parameters->menu-entry entries)
            (grub-configuration-menu-entries config)))

  (define entry->gexp
    (match-lambda
     (($ <menu-entry> label device device-mount-point
                      linux arguments initrd)
      ;; Here DEVICE is the store and DEVICE-MOUNT-POINT is its mount point.
      ;; Use the right file names for LINUX and INITRD in case
      ;; DEVICE-MOUNT-POINT is not "/", meaning that the store is on a
      ;; separate partition.
      (let ((linux  (strip-mount-point device-mount-point linux))
            (initrd (strip-mount-point device-mount-point initrd)))
        #~(format port "menuentry ~s {
  ~a
  linux ~a ~a
  initrd ~a
}~%"
                  #$label
                  #$(grub-root-search device linux)
                  #$linux (string-join (list #$@arguments))
                  #$initrd)))))

  (mlet %store-monad ((sugar (eye-candy config
                                        (menu-entry-device (first all-entries))
                                        (menu-entry-device-mount-point
                                         (first all-entries))
                                        #:system system
                                        #:port #~port)))
    (define builder
      #~(call-with-output-file #$output
          (lambda (port)
            (format port
                    "# This file was generated from your GuixSD configuration.  Any changes
# will be lost upon reconfiguration.
")
            #$sugar
            (format port "
set default=~a
set timeout=~a~%"
                    #$(grub-configuration-default-entry config)
                    #$(grub-configuration-timeout config))
            #$@(map entry->gexp all-entries)

            #$@(if (pair? old-entries)
                   #~((format port "
submenu \"GNU system, old configurations...\" {~%")
                      #$@(map entry->gexp (map boot-parameters->menu-entry old-entries))
                      (format port "}~%"))
                   #~()))))

    (gexp->derivation "grub.cfg" builder)))

;;; grub.scm ends here
