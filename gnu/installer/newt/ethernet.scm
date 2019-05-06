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

(define-module (gnu installer newt ethernet)
  #:use-module (gnu installer connman)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer newt utils)
  #:use-module (gnu installer newt page)
  #:use-module (guix i18n)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (newt)
  #:export (run-ethernet-page))

(define (ethernet-services)
  "Return all the connman services of ethernet type."
  (let ((services (connman-services)))
    (filter (lambda (service)
              (and (string=? (service-type service) "ethernet")
                   (not (string-null? (service-name service)))))
            services)))

(define (ethernet-service->text service)
  "Return a string describing the given ethernet SERVICE."
  (let* ((name (service-name service))
         (path (service-path service))
         (full-name (string-append name "-" path))
         (state (service-state service))
         (connected? (or (string=? state "online")
                         (string=? state "ready"))))
    (format #f "~c ~a~%"
            (if connected? #\* #\ )
            full-name)))

(define (connect-ethernet-service service)
  "Connect to the given ethernet SERVICE. Display a connecting page while the
connection is pending."
  (let* ((service-name (service-name service))
         (form (draw-connecting-page service-name)))
    (connman-connect service)
    (destroy-form-and-pop form)
    service))

(define (run-ethernet-page)
  (let ((services (ethernet-services)))
    (if (null? services)
        (begin
          (run-error-page
           (G_ "No ethernet service available, please try again.")
           (G_ "No service"))
          (raise
           (condition
            (&installer-step-abort))))
        (run-listbox-selection-page
         #:info-text (G_ "Please select an ethernet network.")
         #:title (G_ "Ethernet connection")
         #:listbox-items services
         #:listbox-item->text ethernet-service->text
         #:listbox-height (min (+ (length services) 2) 10)
         #:button-text (G_ "Exit")
         #:button-callback-procedure
         (lambda _
           (raise
            (condition
             (&installer-step-abort))))
         #:listbox-callback-procedure connect-ethernet-service))))
