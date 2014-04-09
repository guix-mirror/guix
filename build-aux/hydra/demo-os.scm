;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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


;;;
;;; This file defines an operating system configuration for the demo virtual
;;; machine images that we build.
;;;

(use-modules (gnu packages zile)
             (gnu packages xorg)
             (gnu packages base)
             (gnu packages admin)
             (gnu packages guile)
             (gnu packages bash)
             (gnu packages linux)
             (gnu packages less)
             (gnu packages tor)
             (gnu packages package-management)

             (gnu system shadow)                  ; 'user-account'
             (gnu services base)
             (gnu services networking)
             (gnu services xorg))

(operating-system
 (host-name "gnu")
 (timezone "Europe/Paris")
 (locale "en_US.UTF-8")
 (users (list (user-account
               (name "guest")
               (uid 1000) (gid 100)
               (comment "Guest of GNU")
               (home-directory "/home/guest"))))
 (services (cons* (slim-service #:auto-login? #t
                                #:default-user "guest")

                  ;; QEMU networking settings.
                  (static-networking-service "eth0" "10.0.2.10"
                                             #:name-servers '("10.0.2.3")
                                             #:gateway "10.0.2.2")

                  %base-services))
 (packages (list bash coreutils findutils grep sed
                 procps psmisc less
                 guile-2.0 dmd guix util-linux inetutils
                 xterm zile)))
