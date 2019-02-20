;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer newt keymap)
  #:use-module (gnu installer keymap)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer newt page)
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (newt)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (run-keymap-page))

(define (run-layout-page layouts layout->text)
  (let ((title (G_ "Layout")))
    (run-listbox-selection-page
     #:title title
     #:info-text (G_ "Please choose your keyboard layout.")
     #:listbox-items layouts
     #:listbox-item->text layout->text
     #:sort-listbox-items? #f
     #:button-text (G_ "Exit")
     #:button-callback-procedure
     (lambda _
       (raise
        (condition
         (&installer-step-abort)))))))

(define (run-variant-page variants variant->text)
  (let ((title (G_ "Variant")))
    (run-listbox-selection-page
     #:title title
     #:info-text (G_ "Please choose a variant for your keyboard layout.")
     #:listbox-items variants
     #:listbox-item->text variant->text
     #:sort-listbox-items? #f
     #:button-text (G_ "Back")
     #:button-callback-procedure
     (lambda _
       (raise
        (condition
         (&installer-step-abort)))))))

(define (sort-layouts layouts)
  "Sort LAYOUTS list by putting the US layout ahead and return it."
  (call-with-values
      (lambda ()
        (partition
         (lambda (layout)
           (let ((name (x11-keymap-layout-name layout)))
             (string=? name "us")))
         layouts))
    (cut append <> <>)))

(define (sort-variants variants)
  "Sort VARIANTS list by putting the international variant ahead and return it."
  (call-with-values
      (lambda ()
        (partition
         (lambda (variant)
           (let ((name (x11-keymap-variant-name variant)))
             (string=? name "altgr-intl")))
         variants))
    (cut append <> <>)))

(define* (run-keymap-page layouts)
  "Run a page asking the user to select a keyboard layout and variant. LAYOUTS
is a list of supported X11-KEYMAP-LAYOUT. Return a list of two elements, the
names of the selected keyboard layout and variant."
  (define keymap-steps
    (list
     (installer-step
      (id 'layout)
      (compute
       (lambda _
         (run-layout-page
          (sort-layouts layouts)
          (lambda (layout)
            (x11-keymap-layout-description layout))))))
     ;; Propose the user to select a variant among those supported by the
     ;; previously selected layout.
     (installer-step
      (id 'variant)
      (compute
       (lambda (result _)
         (let* ((layout (result-step result 'layout))
                (variants (x11-keymap-layout-variants layout)))
           ;; Return #f if the layout does not have any variant.
           (and (not (null? variants))
                (run-variant-page
                 (sort-variants variants)
                 (lambda (variant)
                   (x11-keymap-variant-description
                    variant))))))))))

  (define (format-result result)
    (let ((layout (x11-keymap-layout-name
                   (result-step result 'layout)))
          (variant (and=> (result-step result 'variant)
                          (lambda (variant)
                            (x11-keymap-variant-name variant)))))
      (list layout (or variant ""))))
  (format-result
   (run-installer-steps #:steps keymap-steps)))
