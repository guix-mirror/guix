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

(define-module (gnu installer services)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (system-service?
            system-service-name
            system-service-type
            system-service-snippet

            desktop-system-service?
            networking-system-service?

            %system-services
            system-services->configuration))

(define-record-type* <system-service>
  system-service make-system-service
  system-service?
  (name            system-service-name)           ;string
  (type            system-service-type)           ;'desktop | 'networking
  (snippet         system-service-snippet))       ;sexp

;; This is the list of desktop environments supported as services.
(define %system-services
  (let-syntax ((desktop-environment (syntax-rules ()
                                      ((_ fields ...)
                                       (system-service
                                        (type 'desktop)
                                        fields ...)))))
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
      (snippet '(service enlightenment-desktop-service-type)))

     ;; Networking.
     (system-service
      (name "OpenSSH secure shell daemon (sshd)")
      (type 'networking)
      (snippet '(service openssh-service-type)))
     (system-service
      (name "Tor anonymous network router")
      (type 'networking)
      (snippet '(service tor-service-type))))))

(define (desktop-system-service? service)
  "Return true if SERVICE is a desktop environment service."
  (eq? 'desktop (system-service-type service)))

(define (networking-system-service? service)
  "Return true if SERVICE is a desktop environment service."
  (eq? 'networking (system-service-type service)))

(define (system-services->configuration services)
  "Return the configuration field for SERVICES."
  (let* ((snippets (map system-service-snippet services))
         (desktop? (find desktop-system-service? services))
         (base     (if desktop?
                       '%desktop-services
                       '%base-services)))
    (if (null? snippets)
        `((services ,base))
        `((services (cons* ,@snippets ,base))))))
