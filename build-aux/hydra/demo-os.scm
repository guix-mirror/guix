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
             (gnu system linux)                   ; 'base-pam-services'
             (gnu services base)
             (gnu services networking)
             (gnu services xorg))

(operating-system
 (host-name "gnu")
 (timezone "Europe/Paris")
 (locale "en_US.UTF-8")
 (file-systems
  ;; We provide a dummy file system for /, but that's OK because the VM build
  ;; code will automatically declare the / file system for us.
  (list (file-system
          (mount-point "/")
          (device "dummy")
          (type "dummy"))
        ;; %fuse-control-file-system   ; needs fuse.ko
        %binary-format-file-system))
 (users (list (user-account
               (name "guest")
               (group "wheel")
               (password "")
               (comment "Guest of GNU")
               (home-directory "/home/guest"))))
 (groups (list (user-group (name "root") (id 0))
               (user-group
                (name "wheel")
                (id 1)
                (members '("guest")))             ; allow 'guest' to use sudo
               (user-group
                (name "users")
                (id 100)
                (members '("guest")))))
 (services (cons* (slim-service #:auto-login? #t
                                #:default-user "guest")

                  ;; QEMU networking settings.
                  (static-networking-service "eth0" "10.0.2.10"
                                             #:name-servers '("10.0.2.3")
                                             #:gateway "10.0.2.2")

                  %base-services))
 (pam-services
  ;; Explicitly allow for empty passwords.
  (base-pam-services #:allow-empty-passwords? #t))
 (packages (list bash coreutils findutils grep sed
                 procps psmisc less
                 guile-2.0 dmd guix util-linux inetutils
                 xterm zile)))
