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

(define-module (gnu services desktop)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services avahi)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages wicd)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (dbus-service
            upower-service
            colord-service
            %desktop-services))

;;; Commentary:
;;;
;;; This module contains service definitions for a "desktop" environment.
;;;
;;; Code:


;;;
;;; D-Bus.
;;;

(define (dbus-configuration-directory dbus services)
  "Return a configuration directory for @var{dbus} that includes the
@code{etc/dbus-1/system.d} directories of each package listed in
@var{services}."
  (define build
    #~(begin
        (use-modules (sxml simple)
                     (srfi srfi-1))

        (define (services->sxml services)
          ;; Return the SXML 'includedir' clauses for DIRS.
          `(busconfig
            (servicehelper "/run/setuid-programs/dbus-daemon-launch-helper")
            ,@(append-map (lambda (dir)
                            `((includedir
                               ,(string-append dir "/etc/dbus-1/system.d"))
                              (servicedir         ;for '.service' files
                               ,(string-append dir "/share/dbus-1/services"))
                              (servicedir         ;for '.service' files
                               ,(string-append dir "/share/dbus-1/system-services"))))
                          services)))

        (mkdir #$output)
        (copy-file (string-append #$dbus "/etc/dbus-1/system.conf")
                   (string-append #$output "/system.conf"))

        ;; The default 'system.conf' has an <includedir> clause for
        ;; 'system.d', so create it.
        (mkdir (string-append #$output "/system.d"))

        ;; 'system-local.conf' is automatically included by the default
        ;; 'system.conf', so this is where we stuff our own things.
        (call-with-output-file (string-append #$output "/system-local.conf")
          (lambda (port)
            (sxml->xml (services->sxml (list #$@services))
                       port)))))

  (gexp->derivation "dbus-configuration" build))

(define* (dbus-service services #:key (dbus dbus))
  "Return a service that runs the \"system bus\", using @var{dbus}, with
support for @var{services}.

@uref{http://dbus.freedesktop.org/, D-Bus} is an inter-process communication
facility.  Its system bus is used to allow system services to communicate and
be notified of system-wide events.

@var{services} must be a list of packages that provide an
@file{etc/dbus-1/system.d} directory containing additional D-Bus configuration
and policy files.  For example, to allow avahi-daemon to use the system bus,
@var{services} must be equal to @code{(list avahi)}."
  (mlet %store-monad ((conf (dbus-configuration-directory dbus services)))
    (return
     (service
      (documentation "Run the D-Bus system daemon.")
      (provision '(dbus-system))
      (requirement '(user-processes))
      (start #~(make-forkexec-constructor
                (list (string-append #$dbus "/bin/dbus-daemon")
                      "--nofork"
                      (string-append "--config-file=" #$conf "/system.conf"))))
      (stop #~(make-kill-destructor))
      (user-groups (list (user-group
                          (name "messagebus")
                          (system? #t))))
      (user-accounts (list (user-account
                            (name "messagebus")
                            (group "messagebus")
                            (system? #t)
                            (comment "D-Bus system bus user")
                            (home-directory "/var/run/dbus")
                            (shell
                             #~(string-append #$shadow "/sbin/nologin")))))
      (activate #~(begin
                    (use-modules (guix build utils))

                    (mkdir-p "/var/run/dbus")

                    (let ((user (getpwnam "messagebus")))
                      (chown "/var/run/dbus"
                             (passwd:uid user) (passwd:gid user)))

                    (unless (file-exists? "/etc/machine-id")
                      (format #t "creating /etc/machine-id...~%")
                      (let ((prog (string-append #$dbus "/bin/dbus-uuidgen")))
                        ;; XXX: We can't use 'system' because the initrd's
                        ;; guile system(3) only works when 'sh' is in $PATH.
                        (let ((pid (primitive-fork)))
                          (if (zero? pid)
                              (call-with-output-file "/etc/machine-id"
                                (lambda (port)
                                  (close-fdes 1)
                                  (dup2 (port->fdes port) 1)
                                  (execl prog)))
                              (waitpid pid)))))))))))


;;;
;;; Upower D-Bus service.
;;;

(define* (upower-configuration-file #:key watts-up-pro? poll-batteries?
                                    ignore-lid? use-percentage-for-policy?
                                    percentage-low percentage-critical
                                    percentage-action time-low
                                    time-critical time-action
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
  (mlet %store-monad ((config (upower-configuration-file
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
                             #~(string-append #$shadow "/sbin/nologin")))))))))


;;;
;;; Colord D-Bus service.
;;;

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
                             #~(string-append #$shadow "/sbin/nologin")))))))))

(define %desktop-services
  ;; List of services typically useful for a "desktop" use case.
  (cons* (slim-service)

         (avahi-service)
         (wicd-service)
         (upower-service)
         (colord-service)
         (dbus-service (list avahi wicd upower colord))

         (ntp-service)

         (map (lambda (mservice)
                ;; Provide an nscd ready to use nss-mdns.
                (mlet %store-monad ((service mservice))
                  (if (memq 'nscd (service-provision service))
                      (nscd-service (nscd-configuration)
                                    #:name-services (list nss-mdns))
                      mservice)))
              %base-services)))

;;; desktop.scm ends here
