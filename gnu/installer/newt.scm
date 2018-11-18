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

(define-module (gnu installer newt)
  #:use-module (gnu installer)
  #:use-module (guix discovery)
  #:use-module (guix gexp)
  #:use-module (guix ui)
  #:export (newt-installer))

(define (modules)
  (cons '(newt)
        (scheme-modules*
         (dirname (search-path %load-path "guix.scm"))
         "gnu/installer/newt")))

(define init
  #~(begin
      (newt-init)
      (clear-screen)
      (set-screen-size!)))

(define exit
  #~(begin
      (newt-finish)))

(define exit-error
  #~(lambda (key args)
      (newt-finish)))

(define locale-page
  #~(lambda* (#:key
              supported-locales
              iso639-languages
              iso3166-territories)
      (run-locale-page
       #:supported-locales supported-locales
       #:iso639-languages iso639-languages
       #:iso3166-territories iso3166-territories)))

(define timezone-page
  #~(lambda* (zonetab)
      (run-timezone-page zonetab)))

(define logo
  (string-append
   (dirname (search-path %load-path "guix.scm"))
   "/gnu/installer/aux-files/logo.txt"))

(define welcome-page
  #~(run-welcome-page #$(local-file logo)))

(define menu-page
  #~(lambda (steps)
      (run-menu-page steps)))

(define keymap-page
  #~(lambda* (#:key models layouts)
      (run-keymap-page #:models models
                       #:layouts layouts)))

(define network-page
  #~(run-network-page))

(define hostname-page
  #~(run-hostname-page))

(define user-page
  #~(run-user-page))

(define newt-installer
  (installer
   (name 'newt)
   (modules (modules))
   (init init)
   (exit exit)
   (exit-error exit-error)
   (keymap-page keymap-page)
   (locale-page locale-page)
   (menu-page menu-page)
   (network-page network-page)
   (timezone-page timezone-page)
   (hostname-page hostname-page)
   (user-page user-page)
   (welcome-page welcome-page)))
