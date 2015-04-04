;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu services upower)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages gnome)
  #:use-module (ice-9 match)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:export (upower-service))

;;; Commentary:
;;;
;;; This module provides service definitions for the UPower power and battery
;;; monitoring service.
;;;
;;; Code:

(define* (configuration-file #:key watts-up-pro? poll-batteries? ignore-lid?
                             use-percentage-for-policy? percentage-low
                             percentage-critical percentage-action
                             time-low time-critical time-action
                             critical-power-action)
  "Return an upower-daemon configuration file."
  (define (bool value)
    (if value "true\n" "false\n"))

  (text-file "UPower.conf"
             (string-append
              "[UPower]\n"
              "EnableWattsUpPro=" (bool watts-up-pro?)
              "NoPollBatteries=" (bool (not poll-batteries?))
              "IgnoreLid=" (bool ignore-lid?)
              "UsePercentageForPolicy=" (bool use-percentage-for-policy?)
              "PercentageLow=" (number->string percentage-low) "\n"
              "PercentageCritical=" (number->string percentage-critical) "\n"
              "PercentageAction=" (number->string percentage-action) "\n"
              "TimeLow=" (number->string time-low) "\n"
              "TimeCritical=" (number->string time-critical) "\n"
              "TimeAction=" (number->string time-action) "\n"
              "CriticalPowerAction=" (match critical-power-action
                                       ('hybrid-sleep "HybridSleep")
                                       ('hibernate "Hibernate")
                                       ('power-off "PowerOff"))
              "\n")))

(define* (upower-service #:key (upower upower)
                         (watts-up-pro? #f)
                         (poll-batteries? #t)
                         (ignore-lid? #f)
                         (use-percentage-for-policy? #f)
                         (percentage-low 10)
                         (percentage-critical 3)
                         (percentage-action 2)
                         (time-low 1200)
                         (time-critical 300)
                         (time-action 120)
                         (critical-power-action 'hybrid-sleep))
  "Return a service that runs @uref{http://upower.freedesktop.org/,
@command{upowerd}}, a system-wide monitor for power consumption and battery
levels, with the given configuration settings.  It implements the
@code{org.freedesktop.UPower} D-Bus interface, and is notably used by GNOME."
  (mlet %store-monad ((config (configuration-file
                               #:watts-up-pro? watts-up-pro?
                               #:poll-batteries? poll-batteries?
                               #:ignore-lid? ignore-lid?
                               #:use-percentage-for-policy? use-percentage-for-policy?
                               #:percentage-low percentage-low
                               #:percentage-critical percentage-critical
                               #:percentage-action percentage-action
                               #:time-low time-low
                               #:time-critical time-critical
                               #:time-action time-action
                               #:critical-power-action critical-power-action)))
    (return
     (service
      (documentation "Run the UPower power and battery monitor.")
      (provision '(upower-daemon))
      (requirement '(dbus-system udev))

      (start #~(make-forkexec-constructor
                (list (string-append #$upower "/libexec/upowerd"))
                #:environment-variables
                (list (string-append "UPOWER_CONF_FILE_NAME=" #$config))))
      (stop #~(make-kill-destructor))
      (activate #~(begin
                    (use-modules (guix build utils))
                    (mkdir-p "/var/lib/upower")
                    (let ((user (getpwnam "upower")))
                      (chown "/var/lib/upower"
                             (passwd:uid user) (passwd:gid user)))))

      (user-groups (list (user-group
                          (name "upower")
                          (system? #t))))
      (user-accounts (list (user-account
                            (name "upower")
                            (group "upower")
                            (system? #t)
                            (comment "UPower daemon user")
                            (home-directory "/var/empty")
                            (shell
                             "/run/current-system/profile/sbin/nologin"))))))))

;;; upower.scm ends here
