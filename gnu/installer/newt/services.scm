;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer newt services)
  #:use-module (gnu installer services)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt utils)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (newt)
  #:export (run-services-page))

(define (run-desktop-environments-cbt-page)
  "Run a page allowing the user to choose between various desktop
environments."
  (run-checkbox-tree-page
   #:info-text (G_ "Please select the desktop(s) environment(s) you wish to \
install. If you select multiple desktops environments, you will be able to \
choose the one to use on the log-in screen.")
   #:title (G_ "Desktop environment")
   #:items (filter desktop-system-service? %system-services)
   #:item->text system-service-name               ;no i18n for DE names
   #:checkbox-tree-height 5
   #:exit-button-callback-procedure
   (lambda ()
     (raise
      (condition
       (&installer-step-abort))))))

(define (run-networking-cbt-page network-management?)
  "Run a page allowing the user to select networking services.  When
NETWORK-MANAGEMENT? is true, include network management services like
NetworkManager."
  (run-checkbox-tree-page
   #:info-text (G_ "You can now select networking services to run on your \
system.")
   #:title (G_ "Network service")
   #:items (filter (let ((types (if network-management?
                                    '(network-management networking)
                                    '(networking))))
                     (lambda (service)
                       (memq (system-service-type service) types)))
                   %system-services)
   #:item->text (compose G_ system-service-name)
   #:checkbox-tree-height 5
   #:exit-button-callback-procedure
   (lambda ()
     (raise
      (condition
       (&installer-step-abort))))))

(define (run-services-page)
  (let ((desktop (run-desktop-environments-cbt-page)))
    ;; When the user did not select any desktop services, and thus didn't get
    ;; '%desktop-services', offer network management services.
    (append desktop
            (run-networking-cbt-page (null? desktop)))))
