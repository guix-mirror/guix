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

(define-module (gnu installer record)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (<installer>
            installer
            make-installer
            installer?
            installer-name
            installer-init
            installer-exit
            installer-exit-error
            installer-final-page
            installer-keymap-page
            installer-locale-page
            installer-menu-page
            installer-network-page
            installer-timezone-page
            installer-hostname-page
            installer-user-page
            installer-partition-page
            installer-services-page
            installer-welcome-page
            installer-parameters-menu
            installer-parameters-page))


;;;
;;; Installer record.
;;;

;; The <installer> record contains pages that will be run to prompt the user
;; for the system configuration. The goal of the installer is to produce a
;; complete <operating-system> record and install it.

(define-record-type* <installer>
  installer make-installer
  installer?
  ;; symbol
  (name installer-name)
  ;; procedure: void -> void
  (init installer-init)
  ;; procedure: void -> void
  (exit installer-exit)
  ;; procedure (key arguments) -> void
  (exit-error installer-exit-error)
  ;; procedure void -> void
  (final-page installer-final-page)
  ;; procedure (layouts context) -> (list layout variant)
  (keymap-page installer-keymap-page)
  ;; procedure: (#:key supported-locales iso639-languages iso3166-territories)
  ;; -> glibc-locale
  (locale-page installer-locale-page)
  ;; procedure: (steps) -> step-id
  (menu-page installer-menu-page)
  ;; procedure void -> void
  (network-page installer-network-page)
  ;; procedure (zonetab) -> posix-timezone
  (timezone-page installer-timezone-page)
  ;; procedure void -> void
  (hostname-page installer-hostname-page)
  ;; procedure void -> void
  (user-page installer-user-page)
  ;; procedure void -> void
  (partition-page installer-partition-page)
  ;; procedure void -> void
  (services-page installer-services-page)
  ;; procedure (logo) -> void
  (welcome-page installer-welcome-page)
  ;; procedure (menu-proc) -> void
  (parameters-menu installer-parameters-menu)
  ;; procedure (keyboard-layout-selection) -> void
  (parameters-page installer-parameters-page))
