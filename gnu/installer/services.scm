;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu installer services)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (system-service?
            system-service-name
            system-service-type
            system-service-recommended?
            system-service-snippet
            system-service-packages

            desktop-system-service?

            %system-services
            system-services->configuration))

(define-record-type* <system-service>
  system-service make-system-service
  system-service?
  (name            system-service-name)           ;string
  (type            system-service-type)           ;'desktop|'networking|…
  (recommended?    system-service-recommended?    ;Boolean
                   (default #f))
  (snippet         system-service-snippet         ;list of sexps
                   (default '()))
  (packages        system-service-packages        ;list of sexps
                   (default '())))

(define %system-services
  (let-syntax ((desktop-environment (syntax-rules ()
                                      ((_ fields ...)
                                       (system-service
                                        (type 'desktop)
                                        fields ...))))
               (G_ (syntax-rules ()               ;for xgettext
                     ((_ str) str))))
    (list
     ;; This is the list of desktop environments supported as services.
     (desktop-environment
      (name "GNOME")
      (snippet '((service gnome-desktop-service-type))))
     (desktop-environment
      (name "Xfce")
      (snippet '((service xfce-desktop-service-type))))
     (desktop-environment
      (name "MATE")
      (snippet '((service mate-desktop-service-type))))
     (desktop-environment
      (name "Enlightenment")
      (snippet '((service enlightenment-desktop-service-type))))
     (desktop-environment
      (name "Openbox")
      (packages '((specification->package "openbox"))))
     (desktop-environment
      (name "awesome")
      (packages '((specification->package "awesome"))))
     (desktop-environment
      (name "i3")
      (packages (map (lambda (package)
                       `(specification->package ,package))
                     '("i3-wm" "i3status" "dmenu" "st"))))
     (desktop-environment
      (name "ratpoison")
      (packages '((specification->package "ratpoison")
                  (specification->package "xterm"))))
     (desktop-environment
      (name "Emacs EXWM")
      (packages '((specification->package "emacs")
                  (specification->package "emacs-exwm")
                  (specification->package "emacs-desktop-environment"))))

     ;; Networking.
     (system-service
      (name (G_ "OpenSSH secure shell daemon (sshd)"))
      (type 'networking)
      (snippet '((service openssh-service-type))))
     (system-service
      (name (G_ "Tor anonymous network router"))
      (type 'networking)
      (snippet '((service tor-service-type))))
     (system-service
      (name (G_ "Mozilla NSS certificates, for HTTPS access"))
      (type 'networking)
      (packages '((specification->package "nss-certs")))
      (recommended? #t))

     ;; Miscellaneous system administration services.
     (system-service
       (name (G_ "Network time service (NTP), to set the clock automatically"))
       (type 'administration)
       (recommended? #t)
       (snippet '((service ntp-service-type))))
     (system-service
       (name (G_ "GPM mouse daemon, to use the mouse on the console"))
       (type 'administration)
       (snippet '((service gpm-service-type))))

     ;; Network connectivity management.
     (system-service
      (name (G_ "NetworkManager network connection manager"))
      (type 'network-management)
      (snippet '((service network-manager-service-type)
                 (service wpa-supplicant-service-type))))
     (system-service
      (name (G_ "Connman network connection manager"))
      (type 'network-management)
      (snippet '((service connman-service-type)
                 (service wpa-supplicant-service-type))))
     (system-service
      (name (G_ "DHCP client (dynamic IP address assignment)"))
      (type 'network-management)
      (snippet '((service dhcp-client-service-type))))

     ;; Dealing with documents.
     (system-service
      (name (G_ "CUPS printing system (no Web interface by default)"))
      (type 'document)
      (snippet '((service cups-service-type)))))))

(define (desktop-system-service? service)
  "Return true if SERVICE is a desktop environment service."
  (eq? 'desktop (system-service-type service)))

(define (system-services->configuration services)
  "Return the configuration field for SERVICES."
  (let* ((snippets (append-map system-service-snippet services))
         (packages (append-map system-service-packages services))
         (desktop? (find desktop-system-service? services))
         (base     (if desktop?
                       '%desktop-services
                       '%base-services)))
    (if (null? snippets)
        `(,@(if (null? packages)
                '()
                `((packages (append (list ,@packages)
                                    %base-packages))))
          (services ,base))
        `(,@(if (null? packages)
                '()
                `((packages (append (list ,@packages)
                                    %base-packages))))
          (services (append (list ,@snippets

                                  ,@(if desktop?
                                        ;; XXX: Assume 'keyboard-layout' is in
                                        ;; scope.
                                        '((set-xorg-configuration
                                           (xorg-configuration
                                            (keyboard-layout keyboard-layout))))
                                        '()))
                           ,base))))))
