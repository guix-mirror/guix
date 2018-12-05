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
     #:button-text (G_ "Cancel")
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
     #:button-text (G_ "Back")
     #:button-callback-procedure
     (lambda _
       (raise
        (condition
         (&installer-step-abort)))))))

(define (run-model-page models model->text)
  (let ((title (G_ "Keyboard model selection")))
    (run-listbox-selection-page
     #:title title
     #:info-text (G_ "Please choose your keyboard model.")
     #:listbox-items models
     #:listbox-item->text model->text
     #:listbox-default-item (find (lambda (model)
                                    (string=? (x11-keymap-model-name model)
                                              "pc105"))
                                  models)
     #:sort-listbox-items? #f
     #:button-text (G_ "Back")
     #:button-callback-procedure
     (lambda _
       (raise
        (condition
         (&installer-step-abort)))))))

(define* (run-keymap-page #:key models layouts)
  "Run a page asking the user to select a keyboard model, layout and
variant. MODELS and LAYOUTS are lists of supported X11-KEYMAP-MODEL and
X11-KEYMAP-LAYOUT. Return a list of three elements, the names of the selected
keyboard model, layout and variant."
  (define keymap-steps
    (list
     (installer-step
      (id 'model)
      (compute
       (lambda _
         ;; TODO: Understand why (run-model-page models x11-keymap-model-name)
         ;; fails with: warning: possibly unbound variable
         ;; `%x11-keymap-model-description-procedure.
         (run-model-page models (lambda (model)
                                  (x11-keymap-model-description
                                   model))))))
     (installer-step
      (id 'layout)
      (compute
       (lambda _
         (let* ((layout (run-layout-page
                         layouts
                         (lambda (layout)
                           (x11-keymap-layout-description layout)))))
           (if (null? (x11-keymap-layout-variants layout))
               ;; Break if this layout does not have any variant.
               (raise
                (condition
                 (&installer-step-break)))
               layout)))))
     ;; Propose the user to select a variant among those supported by the
     ;; previously selected layout.
     (installer-step
      (id 'variant)
      (compute
       (lambda (result)
         (let ((variants (x11-keymap-layout-variants
                          (result-step result 'layout))))
           (run-variant-page variants
                             (lambda (variant)
                               (x11-keymap-variant-description
                                variant)))))))))

  (define (format-result result)
    (let ((model (x11-keymap-model-name
                  (result-step result 'model)))
          (layout (x11-keymap-layout-name
                   (result-step result 'layout)))
          (variant (and=> (result-step result 'variant)
                          (lambda (variant)
                            (x11-keymap-variant-name variant)))))
      (list model layout (or variant ""))))
  (format-result
   (run-installer-steps #:steps keymap-steps)))
