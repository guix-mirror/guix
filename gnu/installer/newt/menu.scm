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

(define-module (gnu installer newt menu)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer newt page)
  #:use-module (guix i18n)
  #:use-module (newt)
  #:export (run-menu-page))

(define (run-menu-page steps)
  "Run a menu page, asking the user to select where to resume the install
process from."
  (define (steps->items steps)
    (filter (lambda (step)
              (installer-step-description step))
            steps))

  (run-listbox-selection-page
   #:info-text (G_ "Choose where you want to resume the install.  \
You can also abort the installation by pressing the Abort button.")
   #:title (G_ "Installation menu")
   #:listbox-items (steps->items steps)
   #:listbox-item->text installer-step-description
   #:sort-listbox-items? #f
   #:button-text (G_ "Abort")
   #:button-callback-procedure (lambda ()
                                 (newt-finish)
                                 (primitive-exit 1))))
