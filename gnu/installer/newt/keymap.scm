;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Florian Pelz <pelzflorian@pelzflorian.de>
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
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:export (run-keymap-page
            keyboard-layout->configuration))

(define (run-layout-page layouts layout->text context)
  (let ((title (G_ "Layout")))
    (run-listbox-selection-page
     #:title title
     #:info-text
     (case context
       ((param) (G_ "Please choose your keyboard layout. \
It will only be used during the installation process. \
Non-Latin layouts can be toggled with Alt+Shift."))
       (else (G_ "Please choose your keyboard layout. \
It will be used during the install process, and for the installed system. \
Non-Latin layouts can be toggled with Alt+Shift. You can switch to a \
different layout at any time from the parameters menu.")))
     #:listbox-items layouts
     #:listbox-item->text layout->text
     #:sort-listbox-items? #f
     #:button-text
     (case context
       ((param) (G_ "Continue"))
       (else (G_ "Exit")))
     #:button-callback-procedure
     (case context
       ((param) (const #f))
       (else
        (lambda _
          (abort-to-prompt 'installer-step 'abort)))))))

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
       (abort-to-prompt 'installer-step 'abort)))))

(define (sort-layouts layouts)
  "Sort LAYOUTS list by putting the US layout ahead and return it."
  (define (layout<? layout1 layout2)
    (let ((text1 (x11-keymap-layout-description layout1))
          (text2 (x11-keymap-layout-description layout2)))
      ;; XXX: We're calling 'gettext' more than once per item.
      (string-locale<? (gettext text1 "xkeyboard-config")
                       (gettext text2 "xkeyboard-config"))))

  (define preferred
    ;; Two-letter language tag for the preferred keyboard layout.
    (or (getenv "LANGUAGE") "us"))

  (call-with-values
      (lambda ()
        (partition
         (lambda (layout)
           ;; The 'synopsis' field is usually a language code (e.g., "en")
           ;; while the 'name' field is a country code (e.g., "us").
           (or (string=? (x11-keymap-layout-name layout) preferred)
               (string=? (x11-keymap-layout-synopsis layout) preferred)))
         layouts))
    (lambda (main others)
      (append (sort main layout<?)
              (sort others layout<?)))))

(define (add-empty-variant variants)
  "Prepend #f to VARIANTS so the user has the option to select no variant.
The resulting layout may be different from all other variants (e.g. for
Azerbaijani)."
  (cons #f variants))

(define (sort-variants variants)
  "Sort VARIANTS list by putting the international variant ahead and return it."
  (call-with-values
      (lambda ()
        (partition
         (lambda (variant)
           (and variant
                (let ((name (x11-keymap-variant-name variant)))
                  (string=? name "altgr-intl"))))
         variants))
    (cut append <> <>)))

(define %non-latin-layouts
  ;; List of keyboard layouts marked as $nonlatin in xkeyboard-config.
  ;; See comments in xkeyboard-config file /share/X11/xkb/rules/base.
  ;; We ignore layouts that support Latin input: "kr"
  '("am" "ara" "ben" "bd" "bg" "bt" "by" "cs" "deva" "ge" "gh"
    "gr" "guj" "guru" "il" "in" "ir" "iku" "jp" "kan" "kh"
    "la" "lao" "lk" "mk" "mm" "mn" "mv" "mal" "olck" "ori" "pk"
    "ru" "scc" "sy" "syr" "tel" "th" "tj" "tam" "ua" "uz"
    ;; The list from xkeyboard-config is incomplete.  Add more layouts when
    ;; noticed:
    "et" "kz"))

(define %non-latin-variants
  '("cyrillic"))

(define %latin-layout+variants
  ;; These layout+variant combinations are Latin after all.
  '(("ir" "ku")))

(define (toggleable-latin-layout layout variant)
  "If LAYOUT is a non-Latin layout, return a new combined layout,
a variant, and options that allow the user to switch between the
non-Latin and the Latin layout.  Otherwise, return LAYOUT, VARIANT,
and #f."
  (if (and (not (equal? variant "latin"))
           (not (member (list layout variant) %latin-layout+variants))
           (or (member layout %non-latin-layouts)
               (member variant %non-latin-variants)))
      (let ((latin-layout (if (equal? variant "azerty") "fr" "us")))
        (list
         (string-append layout "," latin-layout)
         ;; Comma to use variant only for non-Latin:
         (and variant (string-append variant ","))
         "grp:alt_shift_toggle"))
      (list layout variant #f)))

(define* (run-keymap-page layouts #:key (context #f))
  "Run a page asking the user to select a keyboard layout and variant. LAYOUTS
is a list of supported X11-KEYMAP-LAYOUT.  For non-Latin keyboard layouts, a
second layout and toggle options will be added automatically.  Return a list
of three elements, the names of the selected keyboard layout, variant and
options."
  (define keymap-steps
    (list
     (installer-step
      (id 'layout)
      (compute
       (lambda _
         (run-layout-page
          (sort-layouts layouts)
          (lambda (layout)
            (gettext (x11-keymap-layout-description layout)
                     "xkeyboard-config"))
          context))))
     ;; Propose the user to select a variant among those supported by the
     ;; previously selected layout.
     (installer-step
      (id 'variant)
      (compute
       (lambda (result _)
         (let* ((layout (result-step result 'layout))
                (variants (if layout
                              (x11-keymap-layout-variants layout)
                              '())))
           ;; Return #f if the layout does not have any variant.
           (and (not (null? variants))
                (run-variant-page
                 (sort-variants (add-empty-variant variants))
                 (lambda (variant)
                   (if variant
                       (gettext (x11-keymap-variant-description variant)
                                "xkeyboard-config")
                       ;; Text to opt for no variant at all:
                       (gettext (x11-keymap-layout-description layout)
                                "xkeyboard-config")))))))))))

  (define (format-result layout variant)
    (let ((layout (x11-keymap-layout-name layout))
          (variant (and=> variant
                          (lambda (variant)
                            (gettext (x11-keymap-variant-name variant)
                                     "xkeyboard-config")))))
      (toggleable-latin-layout layout variant)))

  (let* ((result (run-installer-steps #:steps keymap-steps))
         (layout (result-step result 'layout))
         (variant (result-step result 'variant)))
    (and layout
         (format-result layout variant))))

(define (keyboard-layout->configuration keymap)
  "Return the operating system configuration snippet to install KEYMAP."
  (match keymap
    ((name #f "grp:alt_shift_toggle")
     `((keyboard-layout (keyboard-layout ,name
                                         #:options '("grp:alt_shift_toggle")))))
    ((name #f _)
     `((keyboard-layout (keyboard-layout ,name))))
    ((name variant "grp:alt_shift_toggle")
     `((keyboard-layout (keyboard-layout ,name ,variant
                                         #:options '("grp:alt_shift_toggle")))))
    ((name variant _)
     `((keyboard-layout (keyboard-layout ,name ,variant))))))
