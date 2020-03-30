;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer newt help)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer newt page)
  #:use-module (guix i18n)
  #:use-module (ice-9 match)
  #:use-module (newt)
  #:export (run-help-page))

(define (run-help-page keyboard-layout-selection)
  "Run a help page allowing to change the keyboard layout"
  (let* ((items
          (list
           (cons (G_ "Change keyboard layout") keyboard-layout-selection)))
         (result
          (run-listbox-selection-page
           #:info-text (G_ "This is the help menu, please choose an action.")
           #:title (G_ "Installation help")
           #:listbox-items items
           #:listbox-item->text car
           #:sort-listbox-items? #f
           #:button-text (G_ "Continue"))))
    (match result
      ((_ . proc)
       (proc))
      (_ #f))))
