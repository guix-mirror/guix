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

(define-module (gnu services dbus)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages glib)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:export (dbus-service))

;;; Commentary:
;;;
;;; This module supports the configuration of the D-Bus message bus
;;; (http://dbus.freedesktop.org/).  D-Bus is an inter-process communication
;;; facility.  Its "system bus" is used to allow system services to
;;; communicate and be notified of system-wide events.
;;;
;;; Code:

(define (dbus-configuration-directory dbus services)
  "Return a configuration directory for @var{dbus} that includes the
@code{etc/dbus-1/system.d} directories of each package listed in
@var{services}."
  (define build
    #~(begin
        (use-modules (sxml simple))

        (define (services->sxml services)
          ;; Return the SXML 'includedir' clauses for DIRS.
          `(busconfig
            ,@(map (lambda (dir)
                     `(includedir ,(string-append dir
                                                  "/etc/dbus-1/system.d")))
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
  "Return a service that runs the system bus, using @var{dbus}, with support
for @var{services}.

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
                (string-append #$dbus "/bin/dbus-daemon")
                "--nofork"
                (string-append "--config-file=" #$conf "/system.conf")))
      (stop #~(make-kill-destructor))
      (user-groups (list (user-group
                          (name "messagebus"))))
      (user-accounts (list (user-account
                            (name "messagebus")
                            (group "messagebus")
                            (system? #t)
                            (comment "D-Bus system bus user")
                            (home-directory "/var/run/dbus")
                            (shell
                             "/run/current-system/profile/sbin/nologin"))))
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

;;; dbus.scm ends here
