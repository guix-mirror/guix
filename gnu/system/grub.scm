;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (gnu system file-systems)
  #:autoload   (gnu packages grub) (grub)
  #:autoload   (gnu packages inkscape) (inkscape)
  #:autoload   (gnu packages imagemagick) (imagemagick)
  #:autoload   (gnu packages compression) (gzip)
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

            grub-configuration
            grub-configuration?
            grub-configuration-device

            menu-entry
            menu-entry?

            grub-configuration-file))

;;; Commentary:
;;;
;;; Configuration of GNU GRUB.
;;;
;;; Code:

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
   (file #~(string-append #$%artwork-repository
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
                   (default (@ (gnu packages grub) grub)))
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
  (linux           menu-entry-linux)
  (linux-arguments menu-entry-linux-arguments
                   (default '()))          ; list of string-valued gexps
  (initrd          menu-entry-initrd))     ; file name of the initrd as a gexp


;;;
;;; Background image & themes.
;;;

(define (svg->png svg)
  "Build a PNG from SVG."
  ;; Don't use #:local-build? so that it's substitutable.
  (gexp->derivation "grub-image.png"
                    #~(zero?
                       (system* (string-append #$inkscape "/bin/inkscape")
                                "--without-gui"
                                (string-append "--export-png=" #$output)
                                #$svg))))

(define (resize-image image width height)
  "Resize IMAGE to WIDTHxHEIGHT."
  ;; Don't use #:local-build? so that it's substitutable.
  (let ((size (string-append (number->string width)
                             "x" (number->string height))))
    (gexp->derivation "grub-image.resized.png"
                      #~(zero?
                         (system* (string-append #$imagemagick "/bin/convert")
                                  "-resize" #$size #$image #$output)))))

(define* (grub-background-image config #:key (width 1024) (height 768))
  "Return the GRUB background image defined in CONFIG with a ratio of
WIDTH/HEIGHT, or #f if none was found."
  (let* ((ratio (/ width height))
         (image (find (lambda (image)
                        (= (grub-image-aspect-ratio image) ratio))
                      (grub-theme-images (grub-configuration-theme config)))))
    (if image
        (mlet %store-monad ((png (svg->png (grub-image-file image))))
          (resize-image png width height))
        (with-monad %store-monad
          (return #f)))))

(define (eye-candy config root-fs system port)
  "Return in %STORE-MONAD a gexp that writes to PORT (a port-valued gexp) the
'grub.cfg' part concerned with graphics mode, background images, colors, and
all that.  ROOT-FS is a file-system object denoting the root file system where
the store is.  SYSTEM must be the target system string---e.g.,
\"x86_64-linux\"."
  (define setup-gfxterm-body
    ;; Intel systems need to be switched into graphics mode, whereas most
    ;; other modern architectures have no other mode and therefore don't need
    ;; to be switched.
    (if (string-match "^(x86_64|i[3-6]86)-" system)
        "
  # Leave 'gfxmode' to 'auto'.
  insmod vbe
  insmod vga
  insmod video_bochs
  insmod video_cirrus
  insmod gfxterm
  terminal_output gfxterm
"
        ""))

  (define (theme-colors type)
    (let* ((theme  (grub-configuration-theme config))
           (colors (type theme)))
      (string-append (symbol->string (assoc-ref colors 'fg)) "/"
                     (symbol->string (assoc-ref colors 'bg)))))

  (define font-file
    #~(string-append #$grub "/share/grub/unicode.pf2"))

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
                           #$(grub-root-search root-fs font-file)
                           #$font-file

                           #$image
                           #$(theme-colors grub-theme-color-normal)
                           #$(theme-colors grub-theme-color-highlight))))))


;;;
;;; Configuration file.
;;;

(define (grub-root-search root-fs file)
  "Return the GRUB 'search' command to look for ROOT-FS, which contains FILE,
a gexp.  The result is a gexp that can be inserted in the grub.cfg-generation
code."
  (case (file-system-title root-fs)
    ;; Preferably refer to ROOT-FS by its UUID or label.  This is more
    ;; efficient and less ambiguous, see <>.
    ((uuid)
     (format #f "search --fs-uuid --set ~a"
             (uuid->string (file-system-device root-fs))))
    ((label)
     (format #f "search --label --set ~a"
             (file-system-device root-fs)))
    (else
     ;; As a last resort, look for any device containing FILE.
     #~(format #f "search --file --set ~a" #$file))))

(define* (grub-configuration-file config store-fs entries
                                  #:key
                                  (system (%current-system))
                                  (old-entries '()))
  "Return the GRUB configuration file corresponding to CONFIG, a
<grub-configuration> object, and where the store is available at STORE-FS, a
<file-system> object.  OLD-ENTRIES is taken to be a list of menu entries
corresponding to old generations of the system."
  (define all-entries
    (append entries (grub-configuration-menu-entries config)))

  (define entry->gexp
    (match-lambda
     (($ <menu-entry> label linux arguments initrd)
      #~(format port "menuentry ~s {
  ~a
  linux ~a ~a
  initrd ~a
}~%"
                #$label
                #$(grub-root-search store-fs linux)
                #$linux (string-join (list #$@arguments))
                #$initrd))))

  (mlet %store-monad ((sugar (eye-candy config store-fs system #~port)))
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
                      #$@(map entry->gexp old-entries)
                      (format port "}~%"))
                   #~()))))

    (gexp->derivation "grub.cfg" builder)))

;;; grub.scm ends here
