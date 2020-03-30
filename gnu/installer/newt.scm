;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer newt)
  #:use-module (gnu installer record)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt ethernet)
  #:use-module (gnu installer newt final)
  #:use-module (gnu installer newt help)
  #:use-module (gnu installer newt hostname)
  #:use-module (gnu installer newt keymap)
  #:use-module (gnu installer newt locale)
  #:use-module (gnu installer newt menu)
  #:use-module (gnu installer newt network)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt partition)
  #:use-module (gnu installer newt services)
  #:use-module (gnu installer newt timezone)
  #:use-module (gnu installer newt user)
  #:use-module (gnu installer newt utils)
  #:use-module (gnu installer newt welcome)
  #:use-module (gnu installer newt wifi)
  #:use-module (guix config)
  #:use-module (guix discovery)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-26)
  #:use-module (newt)
  #:export (newt-installer))

(define (init)
  (newt-init)
  (clear-screen)
  (set-screen-size!)
  (push-help-line
   (format #f (G_ "Press <F1> for help."))))

(define (exit)
  (newt-finish)
  (clear-screen))

(define (exit-error file key args)
  (newt-set-color COLORSET-ROOT "white" "red")
  (let ((width (nearest-exact-integer
                (* (screen-columns) 0.8)))
        (height (nearest-exact-integer
                 (* (screen-rows) 0.7))))
    (run-file-textbox-page
     #:info-text (format #f (G_ "The installer has encountered an unexpected \
problem. The backtrace is displayed below. Please report it by email to \
<~a>.") %guix-bug-report-address)
     #:title (G_ "Unexpected problem")
     #:file file
     #:exit-button? #f
     #:info-textbox-width width
     #:file-textbox-width width
     #:file-textbox-height height))
  (newt-set-color COLORSET-ROOT "white" "blue")
  (newt-finish)
  (clear-screen))

(define (final-page result prev-steps)
  (run-final-page result prev-steps))

(define* (locale-page #:key
                      supported-locales
                      iso639-languages
                      iso3166-territories)
  (run-locale-page
   #:supported-locales supported-locales
   #:iso639-languages iso639-languages
   #:iso3166-territories iso3166-territories))

(define (timezone-page zonetab)
  (run-timezone-page zonetab))

(define (welcome-page logo)
  (run-welcome-page logo))

(define (menu-page steps)
  (run-menu-page steps))

(define* (keymap-page layouts context)
  (run-keymap-page layouts #:context context))

(define (network-page)
  (run-network-page))

(define (hostname-page)
  (run-hostname-page))

(define (user-page)
  (run-user-page))

(define (partition-page)
  (run-partioning-page))

(define (services-page)
  (run-services-page))

(define (help-menu menu-proc)
  (newt-set-help-callback menu-proc))

(define (help-page keyboard-layout-selection)
  (run-help-page keyboard-layout-selection))

(define newt-installer
  (installer
   (name 'newt)
   (init init)
   (exit exit)
   (exit-error exit-error)
   (final-page final-page)
   (keymap-page keymap-page)
   (locale-page locale-page)
   (menu-page menu-page)
   (network-page network-page)
   (timezone-page timezone-page)
   (hostname-page hostname-page)
   (user-page user-page)
   (partition-page partition-page)
   (services-page services-page)
   (welcome-page welcome-page)
   (help-menu help-menu)
   (help-page help-page)))
