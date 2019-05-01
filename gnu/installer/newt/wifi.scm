;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Meiyo Peng <meiyo@riseup.net>
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

(define-module (gnu installer newt wifi)
  #:use-module (gnu installer connman)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer newt utils)
  #:use-module (gnu installer newt page)
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (newt)
  #:export (run-wifi-page))

;; This record associates a connman service to its key the listbox.
(define-record-type* <service-item>
  service-item make-service-item
  service-item?
  (service   service-item-service) ; connman <service>
  (key       service-item-key)) ; newt listbox-key

(define (strength->string strength)
  "Convert STRENGTH as an integer percentage into a text printable strength
bar using unicode characters. Taken from NetworkManager's
nmc_wifi_strength_bars."
  (let ((quarter #\x2582)
        (half #\x2584)
        (three-quarter #\x2586)
        (full #\x2588))
    (cond
     ((> strength 80)
      ;; ▂▄▆█
      (string quarter half three-quarter full))
     ((> strength 55)
      ;; ▂▄▆_
      (string quarter half three-quarter #\_))
     ((> strength 30)
      ;; ▂▄__
      (string quarter half #\_ #\_))
     ((> strength 5)
      ;; ▂___
      (string quarter #\_ #\_ #\_))
     (else
      ;; ____
      (string quarter #\_ #\_ #\_ #\_)))))

(define (force-wifi-scan)
  "Force a wifi scan. Raise a condition if no wifi technology is available."
  (let* ((technologies (connman-technologies))
         (wifi-technology
          (find (lambda (technology)
                  (string=? (technology-type technology) "wifi"))
                technologies)))
    (if wifi-technology
        (connman-scan-technology wifi-technology)
        (raise (condition
                (&message
                 (message (G_ "Unable to find a wifi technology"))))))))

(define (draw-scanning-page)
  "Draw a page to indicate a wifi scan in progress."
  (draw-info-page (G_ "Scanning wifi for available networks, please wait.")
                  (G_ "Scan in progress")))

(define (run-wifi-password-page)
  "Run a page prompting user for a password and return it."
  (run-input-page (G_ "Please enter the wifi password.")
                  (G_ "Password required")))

(define (run-wrong-password-page service-name)
  "Run a page to inform user of a wrong password input."
  (run-error-page
   (format #f (G_ "The password you entered for ~a is incorrect.")
           service-name)
   (G_ "Wrong password")))

(define (run-unknown-error-page service-name)
  "Run a page to inform user that a connection error happened."
  (run-error-page
   (format #f
           (G_ "An error occurred while trying to connect to ~a, please retry.")
           service-name)
   (G_ "Connection error")))

(define (password-callback)
  (run-wifi-password-page))

(define (connect-wifi-service listbox service-items)
  "Connect to the wifi service selected in LISTBOX. SERVICE-ITEMS is the list
of <service-item> records present in LISTBOX."
  (let* ((listbox-key (current-listbox-entry listbox))
         (item (find (lambda (item)
                       (eq? (service-item-key item) listbox-key))
                     service-items))
         (service (service-item-service item))
         (service-name (service-name service))
         (form (draw-connecting-page service-name)))
    (dynamic-wind
      (const #t)
      (lambda ()
        (guard (c ((connman-password-error? c)
                   (run-wrong-password-page service-name)
                   #f)
                  ((connman-already-connected-error? c)
                   #t)
                  ((connman-connection-error? c)
                   (run-unknown-error-page service-name)
                   #f))
          (connman-connect-with-auth service password-callback)))
      (lambda ()
        (destroy-form-and-pop form)))))

(define (run-wifi-scan-page)
  "Force a wifi scan and draw a page during the operation."
  (let ((form (draw-scanning-page)))
    (force-wifi-scan)
    (destroy-form-and-pop form)))

(define (wifi-services)
  "Return all the connman services of wifi type."
  (let ((services (connman-services)))
    (filter (lambda (service)
              (and (string=? (service-type service) "wifi")
                   (not (string-null? (service-name service)))))
            services)))

(define* (fill-wifi-services listbox wifi-services)
  "Append all the services in WIFI-SERVICES to the given LISTBOX."
  (clear-listbox listbox)
  (map (lambda (service)
         (let* ((text (service->text service))
                (key (append-entry-to-listbox listbox text)))
           (service-item
            (service service)
            (key key))))
       wifi-services))

;; Maximum length of a wifi service name.
(define service-name-max-length (make-parameter 20))

;; Height of the listbox displaying wifi services.
(define wifi-listbox-height (make-parameter 20))

;; Information textbox width.
(define info-textbox-width (make-parameter 40))

(define (service->text service)
  "Return a string composed of the name and the strength of the given
SERVICE. A '*' preceding the service name indicates that it is connected."
  (let* ((name (service-name service))
         (padded-name (string-pad-right name
                                        (service-name-max-length)))
         (strength (service-strength service))
         (strength-string (strength->string strength))
         (state (service-state service))
         (connected? (or (string=? state "online")
                         (string=? state "ready"))))
    (format #f "~c ~a ~a~%"
            (if connected? #\* #\ )
            padded-name
            strength-string)))

(define (run-wifi-page)
  "Run a page displaying available wifi networks in a listbox. Connect to the
network when the corresponding listbox entry is selected. A button allow to
force a wifi scan."
  (let* ((listbox (make-listbox
                   -1 -1
                   (wifi-listbox-height)
                   (logior FLAG-SCROLL FLAG-BORDER FLAG-RETURNEXIT)))
         (form (make-form))
         (buttons-grid (make-grid 1 1))
         (middle-grid (make-grid 2 1))
         (info-text (G_ "Please select a wifi network."))
         (info-textbox
          (make-reflowed-textbox -1 -1 info-text
                                 (info-textbox-width)
                                 #:flags FLAG-BORDER))
         (exit-button (make-button -1 -1 (G_ "Exit")))
         (scan-button (make-button -1 -1 (G_ "Scan")))
         (services (wifi-services))
         (service-items '()))

    (if (null? services)
        (append-entry-to-listbox listbox (G_ "No wifi detected"))
        (set! service-items (fill-wifi-services listbox services)))

    (set-grid-field middle-grid 0 0 GRID-ELEMENT-COMPONENT listbox)
    (set-grid-field middle-grid 1 0 GRID-ELEMENT-COMPONENT scan-button
                    #:anchor ANCHOR-TOP
                    #:pad-left 2)
    (set-grid-field buttons-grid 0 0 GRID-ELEMENT-COMPONENT exit-button)

    (add-components-to-form form
                            info-textbox
                            listbox scan-button
                            exit-button)
    (make-wrapped-grid-window
     (basic-window-grid info-textbox middle-grid buttons-grid)
     (G_ "Wifi"))

    (receive (exit-reason argument)
        (run-form form)
      (dynamic-wind
        (const #t)
        (lambda ()
          (when (eq? exit-reason 'exit-component)
            (cond
             ((components=? argument scan-button)
              (run-wifi-scan-page)
              (run-wifi-page))
             ((components=? argument exit-button)
              (raise
               (condition
                (&installer-step-abort))))
             ((components=? argument listbox)
              (let ((result (connect-wifi-service listbox service-items)))
                (unless result
                  (run-wifi-page)))))))
        (lambda ()
          (destroy-form-and-pop form))))))
