;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
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

(define-module (gnu services colord)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages gnome)
  #:use-module (ice-9 match)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:export (colord-service))

;;; Commentary:
;;;
;;; This module provides service definitions for the colord color management
;;; service.
;;;
;;; Code:

(define* (colord-service #:key (colord colord))
  "Return a service that runs @command{colord}, a system service with a D-Bus
interface to manage the color profiles of input and output devices such as
screens and scanners.  It is notably used by the GNOME Color Manager graphical
tool.  See @uref{http://www.freedesktop.org/software/colord/, the colord web
site} for more information."
  (with-monad %store-monad
    (return
     (service
      (documentation "Run the colord color management service.")
      (provision '(colord-daemon))
      (requirement '(dbus-system udev))

      (start #~(make-forkexec-constructor
                (list (string-append #$colord "/libexec/colord"))))
      (stop #~(make-kill-destructor))
      (activate #~(begin
                    (use-modules (guix build utils))
                    (mkdir-p "/var/lib/colord")
                    (let ((user (getpwnam "colord")))
                      (chown "/var/lib/colord"
                             (passwd:uid user) (passwd:gid user)))))

      (user-groups (list (user-group
                          (name "colord")
                          (system? #t))))
      (user-accounts (list (user-account
                            (name "colord")
                            (group "colord")
                            (system? #t)
                            (comment "colord daemon user")
                            (home-directory "/var/empty")
                            (shell
                             "/run/current-system/profile/sbin/nologin"))))))))

;;; colord.scm ends here
