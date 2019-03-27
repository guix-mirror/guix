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

(define-module (gnu installer services)
  #:use-module (guix records)
  #:export (<desktop-environment>
            desktop-environment
            make-desktop-environment
            desktop-environment-name
            desktop-environment-snippet

            %desktop-environments
            desktop-environments->configuration))

(define-record-type* <desktop-environment>
  desktop-environment make-desktop-environment
  desktop-environment?
  (name            desktop-environment-name) ;string
  (snippet         desktop-environment-snippet)) ;symbol

;; This is the list of desktop environments supported as services.
(define %desktop-environments
  (list
   (desktop-environment
    (name "GNOME")
    (snippet '(service gnome-desktop-service-type)))
   (desktop-environment
    (name "Xfce")
    ;; TODO: Use 'xfce-desktop-service-type' when the 'guix' package provides
    ;; it with a default value.
    (snippet '(xfce-desktop-service)))
   (desktop-environment
    (name "MATE")
    (snippet '(service mate-desktop-service-type)))
   (desktop-environment
    (name "Enlightenment")
    (snippet '(service enlightenment-desktop-service-type)))))

(define (desktop-environments->configuration desktop-environments)
  "Return the configuration field for DESKTOP-ENVIRONMENTS."
  (let ((snippets
         (map desktop-environment-snippet desktop-environments)))
    `(,@(if (null? snippets)
            '()
            `((services (cons* ,@snippets
                               %desktop-services)))))))
