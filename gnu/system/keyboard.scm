;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu system keyboard)
  #:use-module (guix gexp)
  #:use-module ((gnu packages xorg)
                #:select (xkeyboard-config console-setup))
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:export (keyboard-layout?
            keyboard-layout
            keyboard-layout-name
            keyboard-layout-variant
            keyboard-layout-model
            keyboard-layout-options

            keyboard-layout->console-keymap))

;;; Commentary:
;;;
;;; This module provides a data structure to represent keyboard layouts
;;; according to the XKB naming and classification (see the 'xkeyboard-config'
;;; package).
;;;
;;; Code:

(define-immutable-record-type <keyboard-layout>
  (%keyboard-layout name variant model options)
  keyboard-layout?
  (name      keyboard-layout-name)                ;string
  (variant   keyboard-layout-variant)             ;#f | string
  (model     keyboard-layout-model)               ;#f | string
  (options   keyboard-layout-options))            ;list of strings

(define* (keyboard-layout name #:optional variant
                          #:key model (options '()))
  "Return a new keyboard layout with the given NAME and VARIANT.

NAME must be a string such as \"fr\"; VARIANT must be a string such as
\"bepo\" or \"nodeadkeys\".  See the 'xkeyboard-config' package for valid
options."
  (%keyboard-layout name variant model options))

(define* (keyboard-layout->console-keymap layout
                                          #:key
                                          (xkeyboard-config xkeyboard-config))
  "Return a Linux console keymap file for LAYOUT, a <keyboard-layout> record.
Layout information is taken from the XKEYBOARD-CONFIG package."
  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 popen)
                       (ice-9 match))

          (define pipe
            (open-pipe* OPEN_READ
                        #+(file-append console-setup "/bin/ckbcomp")
                        (string-append "-I"
                                       #+(file-append xkeyboard-config
                                                      "/share/X11/xkb"))
                        "-rules" "base"
                        #$@(match (keyboard-layout-model layout)
                             (#f      '())
                             (model   `("-model" ,model)))
                        #$(keyboard-layout-name layout)
                        #$(or (keyboard-layout-variant layout)
                              "")
                        #$(string-join (keyboard-layout-options layout) ",")))

          (call-with-output-file #$output
            (lambda (output)
              (dump-port pipe output)))

          ;; Note: ckbcomp errors out when the layout name is unknown, but
          ;; merely emits a warning when the variant is unknown.
          (unless (zero? (close-pipe pipe))
            (error "failed to create console keymap for keyboard layout"
                   #$(keyboard-layout-name layout))))))

  (computed-file (string-append "console-keymap."
                                (keyboard-layout-name layout))
                 build))
