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

(define-module (gnu installer newt utils)
  #:use-module (ice-9 receive)
  #:use-module (newt)
  #:export (screen-columns
            screen-rows

            destroy-form-and-pop
            set-screen-size!))

;; Number of columns and rows of the terminal.
(define screen-columns (make-parameter 0))
(define screen-rows    (make-parameter 0))

(define (destroy-form-and-pop form)
  "Destroy the given FORM and pop the current window."
  (destroy-form form)
  (pop-window))

(define (set-screen-size!)
  "Set the parameters 'screen-columns' and 'screen-rows' to the number of
columns and rows respectively of the current terminal."
  (receive (columns rows)
      (screen-size)
    (screen-columns columns)
    (screen-rows rows)))
