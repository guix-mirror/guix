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

(define-module (gnu installer keymap)
  #:use-module (guix records)
  #:use-module (sxml match)
  #:use-module (sxml simple)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (<x11-keymap-model>
            x11-keymap-model
            make-x11-keymap-model
            x11-keymap-model?
            x11-keymap-model-name
            x11-keymap-model-description

            <x11-keymap-layout>
            x11-keymap-layout
            make-x11-keymap-layout
            x11-keymap-layout?
            x11-keymap-layout-name
            x11-keymap-layout-synopsis
            x11-keymap-layout-description
            x11-keymap-layout-variants

            <x11-keymap-variant>
            x11-keymap-variant
            make-x11-keymap-variant
            x11-keymap-variant?
            x11-keymap-variant-name
            x11-keymap-variant-description

            default-keyboard-model
            xkb-rules->models+layouts
            kmscon-update-keymap))

(define-record-type* <x11-keymap-model>
  x11-keymap-model make-x11-keymap-model
  x11-keymap-model?
  (name            x11-keymap-model-name) ;string
  (description     x11-keymap-model-description)) ;string

(define-record-type* <x11-keymap-layout>
  x11-keymap-layout make-x11-keymap-layout
  x11-keymap-layout?
  (name            x11-keymap-layout-name) ;string
  (synopsis        x11-keymap-layout-synopsis)    ;string (e.g., "en")
  (description     x11-keymap-layout-description) ;string (a whole phrase)
  (variants        x11-keymap-layout-variants)) ;list of <x11-keymap-variant>

(define-record-type* <x11-keymap-variant>
  x11-keymap-variant make-x11-keymap-variant
  x11-keymap-variant?
  (name            x11-keymap-variant-name) ;string
  (description     x11-keymap-variant-description)) ;string

;; Assume all modern keyboards have this model.
(define default-keyboard-model (make-parameter "pc105"))

(define (xkb-rules->models+layouts file)
  "Parse FILE and return two values, the list of supported X11-KEYMAP-MODEL
and X11-KEYMAP-LAYOUT records. FILE is an XML file from the X Keyboard
Configuration Database, describing possible XKB configurations."
  (define (model m)
    (sxml-match m
                [(model
                  (configItem
                   (name ,name)
                   (description ,description)
                   . ,rest))
                 (x11-keymap-model
                  (name name)
                  (description description))]))

  (define (variant v)
    (sxml-match v
                [(variant
                  ;; According to xbd-rules DTD, the definition of a
                  ;; configItem is: <!ELEMENT configItem
                  ;; (name,shortDescription*,description*,vendor?,
                  ;; countryList?,languageList?,hwList?)>
                  ;;
                  ;; shortDescription and description are optional elements
                  ;; but sxml-match does not support default values for
                  ;; elements (only attributes). So to avoid writing as many
                  ;; patterns as existing possibilities, gather all the
                  ;; remaining elements but name in REST-VARIANT.
                  (configItem
                   (name ,name)
                   . ,rest-variant))
                 (x11-keymap-variant
                  (name name)
                  (description (car
                                (assoc-ref rest-variant 'description))))]))

  (define (layout l)
    (sxml-match l
                [(layout
                  (configItem
                   (name ,name)
                   . ,rest-layout)
                  (variantList ,[variant -> v] ...))
                 (x11-keymap-layout
                  (name name)
                  (synopsis (car
                             (assoc-ref rest-layout 'shortDescription)))
                  (description (car
                                (assoc-ref rest-layout 'description)))
                  (variants (list v ...)))]
                [(layout
                  (configItem
                   (name ,name)
                   . ,rest-layout))
                 (x11-keymap-layout
                  (name name)
                  (synopsis (car
                             (assoc-ref rest-layout 'shortDescription)))
                  (description (car
                                (assoc-ref rest-layout 'description)))
                  (variants '()))]))

  (let ((sxml (call-with-input-file file
                (lambda (port)
                  (xml->sxml port #:trim-whitespace? #t)))))
    (match
        (sxml-match sxml
                    [(*TOP*
                      ,pi
                      (xkbConfigRegistry
                       (@ . ,ignored)
                       (modelList ,[model -> m] ...)
                       (layoutList ,[layout -> l] ...)
                       . ,rest))
                     (list
                      (list m ...)
                      (list l ...))])
      ((models layouts)
       (values models layouts)))))

(define (kmscon-update-keymap model layout variant)
  "Update kmscon keymap with the provided MODEL, LAYOUT and VARIANT."
  (and=>
   (getenv "KEYMAP_UPDATE")
   (lambda (keymap-file)
     (unless (file-exists? keymap-file)
       (error "Unable to locate keymap update file"))

     ;; See file gnu/packages/patches/kmscon-runtime-keymap-switch.patch.
     ;; This dirty hack makes possible to update kmscon keymap at runtime by
     ;; writing an X11 keyboard model, layout and variant to a named pipe
     ;; referred by KEYMAP_UPDATE environment variable.
     (call-with-output-file keymap-file
       (lambda (port)
         (format port model)
         (put-u8 port 0)

         (format port layout)
         (put-u8 port 0)

         (format port variant)
         (put-u8 port 0))))))
