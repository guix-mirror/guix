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

(define-module (gnu installer newt network)
  #:use-module (gnu installer connman)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt ethernet)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt wifi)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (newt)
  #:export (run-network-page))

;; Maximum length of a technology name.
(define technology-name-max-length (make-parameter 20))

(define (technology->text technology)
  "Return a string describing the given TECHNOLOGY."
  (let* ((name (technology-name technology))
         (padded-name (string-pad-right name
                                        (technology-name-max-length))))
    (format #f "~a~%" padded-name)))

(define (run-technology-page)
  "Run a page to ask the user which technology shall be used to access
Internet and return the selected technology. For now, only technologies with
\"ethernet\" or \"wifi\" types are supported."
  (define (technology-items)
    (filter (lambda (technology)
              (let ((type (technology-type technology)))
                (or
                 (string=? type "ethernet")
                 (string=? type "wifi"))))
            (connman-technologies)))

  (let ((items (technology-items)))
    (if (null? items)
        (case (choice-window
               (G_ "Internet access")
               (G_ "Continue")
               (G_ "Exit")
               (G_ "The install process requires an internet access, but no \
network device were found. Do you want to continue anyway?"))
          ((1) (raise
                (condition
                 (&installer-step-break))))
          ((2) (raise
                (condition
                 (&installer-step-abort)))))
        (run-listbox-selection-page
         #:info-text (G_ "The install process requires an internet access.\
 Please select a network device.")
         #:title (G_ "Internet access")
         #:listbox-items items
         #:listbox-item->text technology->text
         #:button-text (G_ "Exit")
         #:button-callback-procedure
         (lambda _
           (raise
            (condition
             (&installer-step-abort))))))))

(define (find-technology-by-type technologies type)
  "Find and return a technology with the given TYPE in TECHNOLOGIES list."
  (find (lambda (technology)
          (string=? (technology-type technology)
                    type))
        technologies))

(define (wait-technology-powered technology)
  "Wait and display a progress bar until the given TECHNOLOGY is powered."
  (let ((name (technology-name technology))
        (full-value 5))
    (run-scale-page
     #:title (G_ "Powering technology")
     #:info-text (format #f "Waiting for technology ~a to be powered." name)
     #:scale-full-value full-value
     #:scale-update-proc
     (lambda (value)
       (let* ((technologies (connman-technologies))
              (type (technology-type technology))
              (updated-technology
               (find-technology-by-type technologies type))
              (technology-powered? updated-technology))
         (sleep 1)
         (if technology-powered?
             full-value
             (+ value 1)))))))

(define (wait-service-online)
  "Display a newt scale until connman detects an Internet access. Do
FULL-VALUE tentatives, spaced by 1 second."
  (let* ((full-value 5))
    (run-scale-page
     #:title (G_ "Checking connectivity")
     #:info-text (G_ "Waiting for Internet access establishment...")
     #:scale-full-value full-value
     #:scale-update-proc
     (lambda (value)
       (sleep 1)
       (if (connman-online?)
           full-value
           (+ value 1))))
    (unless (connman-online?)
      (run-error-page
       (G_ "The selected network does not provide access to the \
Internet, please try again.")
       (G_ "Connection error"))
      (raise
       (condition
        (&installer-step-abort))))))

(define (run-network-page)
  "Run a page to allow the user to configure connman so that it can access the
Internet."
  (define network-steps
    (list
     ;; Ask the user to choose between ethernet and wifi technologies.
     (installer-step
      (id 'select-technology)
      (compute
       (lambda _
         (run-technology-page))))
     ;; Enable the previously selected technology.
     (installer-step
      (id 'power-technology)
      (compute
       (lambda (result _)
         (let ((technology (result-step result 'select-technology)))
           (connman-enable-technology technology)
           (wait-technology-powered technology)))))
     ;; Propose the user to connect to one of the service available for the
     ;; previously selected technology.
     (installer-step
      (id 'connect-service)
      (compute
       (lambda (result _)
         (let* ((technology (result-step result 'select-technology))
                (type (technology-type technology)))
           (cond
            ((string=? "wifi" type)
             (run-wifi-page))
            ((string=? "ethernet" type)
             (run-ethernet-page)))))))
     ;; Wait for connman status to switch to 'online, which means it can
     ;; access Internet.
     (installer-step
      (id 'wait-online)
      (compute (lambda _
                 (wait-service-online))))))
  (run-installer-steps
   #:steps network-steps
   #:rewind-strategy 'start))
