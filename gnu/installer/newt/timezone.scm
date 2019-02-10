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

(define-module (gnu installer newt timezone)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer timezone)
  #:use-module (gnu installer newt page)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (newt)
  #:export (run-timezone-page))

;; Height of the listbox displaying timezones.
(define timezone-listbox-heigth (make-parameter 20))

;; Information textbox width.
(define info-textbox-width (make-parameter 40))

(define (fill-timezones listbox timezones)
  "Fill the given LISTBOX with TIMEZONES. Return an association list
correlating listbox keys with timezones."
  (map (lambda (timezone)
         (let ((key (append-entry-to-listbox listbox timezone)))
           (cons key timezone)))
       timezones))

(define (run-timezone-page zonetab)
  "Run a page displaying available timezones, grouped by regions. The user is
invited to select a timezone. The selected timezone, under Posix format is
returned."
  (define (all-but-last list)
    (reverse (cdr (reverse list))))

  (define (run-page timezone-tree)
    (define (loop path)
      (let ((timezones (locate-children timezone-tree path)))
        (run-listbox-selection-page
         #:title (G_ "Timezone")
         #:info-text (G_ "Please select a timezone.")
         #:listbox-items timezones
         #:listbox-item->text identity
         #:button-text (if (null? path)
                           (G_ "Exit")
                           (G_ "Back"))
         #:button-callback-procedure
         (if (null? path)
             (lambda _
               (raise
                (condition
                 (&installer-step-abort))))
             (lambda _
               (loop (all-but-last path))))
         #:listbox-callback-procedure
         (lambda (timezone)
           (let* ((timezone* (append path (list timezone)))
                  (tz (timezone->posix-tz timezone*)))
             (if (timezone-has-child? timezone-tree timezone*)
                 (loop timezone*)
                 tz))))))
    (loop '()))

  (let ((timezone-tree (zonetab->timezone-tree zonetab)))
    (run-page timezone-tree)))
