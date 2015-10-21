;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
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
  #:use-module (gnu services dmd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages admin)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (dbus-root-service-type
            dbus-service))

;;;
;;; D-Bus.
;;;

(define-record-type* <dbus-configuration>
  dbus-configuration make-dbus-configuration
  dbus-configuration?
  (dbus      dbus-configuration-dbus              ;<package>
             (default dbus))
  (services  dbus-configuration-services          ;list of <package>
             (default '())))

(define (dbus-configuration-directory services)
  "Return a directory contains the @code{system-local.conf} file for DBUS that
includes the @code{etc/dbus-1/system.d} directories of each package listed in
@var{services}."
  (define build
    #~(begin
        (use-modules (sxml simple)
                     (srfi srfi-1))

        (define (services->sxml services)
          ;; Return the SXML 'includedir' clauses for DIRS.
          `(busconfig
            ,@(append-map (lambda (dir)
                            `((includedir
                               ,(string-append dir "/etc/dbus-1/system.d"))
                              (servicedir         ;for '.service' files
                               ,(string-append dir "/share/dbus-1/services"))
                              (servicedir       ;likewise, for auto-activation
                               ,(string-append
                                 dir
                                 "/share/dbus-1/system-services"))))
                          services)))

        (mkdir #$output)
        ;; 'system-local.conf' is automatically included by the default
        ;; 'system.conf', so this is where we stuff our own things.
        (call-with-output-file (string-append #$output "/system-local.conf")
          (lambda (port)
            (sxml->xml (services->sxml (list #$@services))
                       port)))))

  (computed-file "dbus-configuration" build))

(define (dbus-etc-files config)
  "Return a list of FILES for @var{etc-service-type} to build the
@code{/etc/dbus-1} directory."
  (list `("dbus-1" ,(dbus-configuration-directory
                     (dbus-configuration-services config)))))

(define %dbus-accounts
  ;; Accounts used by the system bus.
  (list (user-group (name "messagebus") (system? #t))
        (user-account
         (name "messagebus")
         (group "messagebus")
         (system? #t)
         (comment "D-Bus system bus user")
         (home-directory "/var/run/dbus")
         (shell #~(string-append #$shadow "/sbin/nologin")))))

(define (dbus-activation config)
  "Return an activation gexp for D-Bus using @var{config}."
  #~(begin
      (use-modules (guix build utils))

      (mkdir-p "/var/run/dbus")

      (let ((user (getpwnam "messagebus")))
        (chown "/var/run/dbus"
               (passwd:uid user) (passwd:gid user)))

      (unless (file-exists? "/etc/machine-id")
        (format #t "creating /etc/machine-id...~%")
        (let ((prog (string-append #$(dbus-configuration-dbus config)
                                   "/bin/dbus-uuidgen")))
          ;; XXX: We can't use 'system' because the initrd's
          ;; guile system(3) only works when 'sh' is in $PATH.
          (let ((pid (primitive-fork)))
            (if (zero? pid)
                (call-with-output-file "/etc/machine-id"
                  (lambda (port)
                    (close-fdes 1)
                    (dup2 (port->fdes port) 1)
                    (execl prog)))
                (waitpid pid)))))))

(define dbus-dmd-service
  (match-lambda
    (($ <dbus-configuration> dbus)
     (list (dmd-service
            (documentation "Run the D-Bus system daemon.")
            (provision '(dbus-system))
            (requirement '(user-processes))
            (start #~(make-forkexec-constructor
                      (list (string-append #$dbus "/bin/dbus-daemon")
                            "--nofork" "--system")))
            (stop #~(make-kill-destructor)))))))

(define dbus-root-service-type
  (service-type (name 'dbus)
                (extensions
                 (list (service-extension dmd-root-service-type
                                          dbus-dmd-service)
                       (service-extension activation-service-type
                                          dbus-activation)
                       (service-extension etc-service-type
                                          dbus-etc-files)
                       (service-extension account-service-type
                                          (const %dbus-accounts))))

                ;; Extensions consist of lists of packages (representing D-Bus
                ;; services) that we just concatenate.
                ;;
                ;; FIXME: We need 'dbus-daemon-launch-helper' to be
                ;; setuid-root for auto-activation to work.
                (compose concatenate)

                ;; The service's parameters field is extended by augmenting
                ;; its <dbus-configuration> 'services' field.
                (extend (lambda (config services)
                          (dbus-configuration
                           (inherit config)
                           (services
                            (append (dbus-configuration-services config)
                                    services)))))))

(define* (dbus-service #:key (dbus dbus) (services '()))
  "Return a service that runs the \"system bus\", using @var{dbus}, with
support for @var{services}.

@uref{http://dbus.freedesktop.org/, D-Bus} is an inter-process communication
facility.  Its system bus is used to allow system services to communicate and
be notified of system-wide events.

@var{services} must be a list of packages that provide an
@file{etc/dbus-1/system.d} directory containing additional D-Bus configuration
and policy files.  For example, to allow avahi-daemon to use the system bus,
@var{services} must be equal to @code{(list avahi)}."
  (service dbus-root-service-type
           (dbus-configuration (dbus dbus)
                               (services services))))

;;; dbus.scm ends here
