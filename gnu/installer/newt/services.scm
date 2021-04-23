;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
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
  (let ((items (filter desktop-system-service? %system-services)))
    (run-checkbox-tree-page
     #:info-text (G_ "Please select the desktop environment(s) you wish to \
install.  If you select multiple desktop environments here, you will be able \
to choose from them later when you log in.")
     #:title (G_ "Desktop environment")
     #:items items
     #:selection (map system-service-recommended? items)
     #:item->text system-service-name             ;no i18n for DE names
     #:checkbox-tree-height 9
     #:exit-button-callback-procedure
     (lambda ()
       (raise
        (condition
         (&installer-step-abort)))))))

(define (run-networking-cbt-page)
  "Run a page allowing the user to select networking services."
  (let ((items (filter (lambda (service)
                         (eq? 'networking (system-service-type service)))
                       %system-services)))
    (run-checkbox-tree-page
     #:info-text (G_ "You can now select networking services to run on your \
system.")
     #:title (G_ "Network service")
     #:items items
     #:selection (map system-service-recommended? items)
     #:item->text (compose G_ system-service-name)
     #:checkbox-tree-height 5
     #:exit-button-callback-procedure
     (lambda ()
       (raise
        (condition
         (&installer-step-abort)))))))

(define (run-printing-services-cbt-page)
  "Run a page allowing the user to select document services such as CUPS."
  (let ((items (filter (lambda (service)
                         (eq? 'document
                              (system-service-type service)))
                       %system-services)))
    (run-checkbox-tree-page
     #:info-text (G_ "You can now select the CUPS printing service to run on your \
system.")
     #:title (G_ "Printing and document services")
     #:items items
     #:selection (map system-service-recommended? items)
     #:item->text (compose G_ system-service-name)
     #:checkbox-tree-height 9
     #:exit-button-callback-procedure
     (lambda ()
       (raise
        (condition
         (&installer-step-abort)))))))

(define (run-console-services-cbt-page)
  "Run a page to select various system adminstration services for non-graphical
systems."
  (let ((items (filter (lambda (service)
                         (eq? 'administration
                              (system-service-type service)))
                       %system-services)))
    (run-checkbox-tree-page
      #:title (G_ "Console services")
      #:info-text (G_ "Select miscellaneous services to run on your \
non-graphical system.")
      #:items items
      #:selection (map system-service-recommended? items)
      #:item->text (compose G_ system-service-name)
      #:checkbox-tree-height 5
      #:exit-button-callback-procedure
      (lambda ()
        (raise
          (condition
            (&installer-step-abort)))))))

(define (run-network-management-page)
  "Run a page to select among several network management methods."
  (let ((title (G_ "Network management")))
    (run-listbox-selection-page
     #:title title
     #:info-text (G_ "Choose the method to manage network connections.

We recommend NetworkManager or Connman for a WiFi-capable laptop; the DHCP \
client may be enough for a server.")
     #:info-textbox-width 70
     #:listbox-height 7
     #:listbox-items (filter (lambda (service)
                               (eq? 'network-management
                                    (system-service-type service)))
                             %system-services)
     #:listbox-item->text (compose G_ system-service-name)
     #:sort-listbox-items? #f
     #:button-text (G_ "Exit")
     #:button-callback-procedure
     (lambda _
       (raise
        (condition
         (&installer-step-abort)))))))

(define (run-services-page)
  (let ((desktop (run-desktop-environments-cbt-page)))
    ;; When the user did not select any desktop services, and thus didn't get
    ;; '%desktop-services', offer network management services.
    (append desktop
            (run-networking-cbt-page)
            (if (null? desktop)
                (cons (run-network-management-page)
                      (run-console-services-cbt-page))
                '())
            (run-printing-services-cbt-page))))
