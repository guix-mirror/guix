;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module ((gnu packages glib) #:select (dbus))
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages admin)
  #:use-module (guix gexp)
  #:use-module ((guix packages) #:select (package-name))
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (dbus-configuration
            dbus-configuration?
            dbus-root-service-type
            dbus-service
            wrapped-dbus-service

            polkit-service-type
            polkit-service))

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

(define (system-service-directory services)
  "Return the system service directory, containing @code{.service} files for
all the services that may be activated by the daemon."
  (computed-file "dbus-system-services"
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils)
                                    (srfi srfi-1))

                       (define files
                         (append-map (lambda (service)
                                       (find-files
                                        (string-append
                                         service
                                         "/share/dbus-1/")
                                        "\\.service$"))
                                     (list #$@services)))

                       (mkdir #$output)
                       (for-each (lambda (file)
                                   (symlink file
                                            (string-append #$output "/"
                                                           (basename file))))
                                 files)
                       #t))))

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
            (servicehelper "/run/setuid-programs/dbus-daemon-launch-helper")

            ;; First, the '.service' files of services subject to activation.
            ;; We use a fixed location under /etc because the setuid helper
            ;; looks for them in that location and nowhere else.  See
            ;; <https://bugs.freedesktop.org/show_bug.cgi?id=92458>.
            (servicedir "/etc/dbus-1/system-services")

            ,@(append-map (lambda (dir)
                            `((includedir
                               ,(string-append dir "/etc/dbus-1/system.d"))
                              (servicedir       ;for '.service' files
                               ,(string-append dir "/share/dbus-1/services"))))
                          services)))

        (mkdir #$output)

        ;; Provide /etc/dbus-1/system-services, which is where the setuid
        ;; helper looks for system service files.
        (symlink #$(system-service-directory services)
                 (string-append #$output "/system-services"))

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
         (shell (file-append shadow "/sbin/nologin")))))

(define dbus-setuid-programs
  ;; Return the file name of the setuid program that we need.
  (match-lambda
    (($ <dbus-configuration> dbus services)
     (list (file-append dbus "/libexec/dbus-daemon-launch-helper")))))

(define (dbus-activation config)
  "Return an activation gexp for D-Bus using @var{config}."
  #~(begin
      (use-modules (guix build utils))

      (mkdir-p "/var/run/dbus")

      (let ((user (getpwnam "messagebus")))
        (chown "/var/run/dbus"
               (passwd:uid user) (passwd:gid user))

        ;; This directory contains the daemon's socket so it must be
        ;; world-readable.
        (chmod "/var/run/dbus" #o755))

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

(define dbus-shepherd-service
  (match-lambda
    (($ <dbus-configuration> dbus)
     (list (shepherd-service
            (documentation "Run the D-Bus system daemon.")
            (provision '(dbus-system))
            (requirement '(user-processes))
            (start #~(make-forkexec-constructor
                      (list (string-append #$dbus "/bin/dbus-daemon")
                            "--nofork" "--system")
                      #:pid-file "/var/run/dbus/pid"))
            (stop #~(make-kill-destructor)))))))

(define dbus-root-service-type
  (service-type (name 'dbus)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          dbus-shepherd-service)
                       (service-extension activation-service-type
                                          dbus-activation)
                       (service-extension etc-service-type
                                          dbus-etc-files)
                       (service-extension account-service-type
                                          (const %dbus-accounts))
                       (service-extension setuid-program-service-type
                                          dbus-setuid-programs)))

                ;; Extensions consist of lists of packages (representing D-Bus
                ;; services) that we just concatenate.
                (compose concatenate)

                ;; The service's parameters field is extended by augmenting
                ;; its <dbus-configuration> 'services' field.
                (extend (lambda (config services)
                          (dbus-configuration
                           (inherit config)
                           (services
                            (append (dbus-configuration-services config)
                                    services)))))

                (default-value (dbus-configuration))))

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

(define (wrapped-dbus-service service program variable value)
  "Return a wrapper for @var{service}, a package containing a D-Bus service,
where @var{program} is wrapped such that environment variable @var{variable}
is set to @var{value} when the bus daemon launches it."
  (define wrapper
    (program-file (string-append (package-name service) "-program-wrapper")
                  #~(begin
                      (setenv #$variable #$value)
                      (apply execl (string-append #$service "/" #$program)
                             (string-append #$service "/" #$program)
                             (cdr (command-line))))))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))

          (define service-directory
            "/share/dbus-1/system-services")

          (mkdir-p (dirname (string-append #$output
                                           service-directory)))
          (copy-recursively (string-append #$service
                                           service-directory)
                            (string-append #$output
                                           service-directory))
          (symlink (string-append #$service "/etc") ;for etc/dbus-1
                   (string-append #$output "/etc"))

          (for-each (lambda (file)
                      (substitute* file
                        (("Exec[[:blank:]]*=[[:blank:]]*([[:graph:]]+)(.*)$"
                          _ original-program arguments)
                         (string-append "Exec=" #$wrapper arguments
                                        "\n"))))
                    (find-files #$output "\\.service$")))))

  (computed-file (string-append (package-name service) "-wrapper")
                 build))


;;;
;;; Polkit privilege management service.
;;;

(define-record-type* <polkit-configuration>
  polkit-configuration make-polkit-configuration
  polkit-configuration?
  (polkit   polkit-configuration-polkit           ;<package>
            (default polkit))
  (actions  polkit-configuration-actions          ;list of <package>
            (default '())))

(define %polkit-accounts
  (list (user-group (name "polkitd") (system? #t))
        (user-account
         (name "polkitd")
         (group "polkitd")
         (system? #t)
         (comment "Polkit daemon user")
         (home-directory "/var/empty")
         (shell "/run/current-system/profile/sbin/nologin"))))

(define %polkit-pam-services
  (list (unix-pam-service "polkit-1")))

(define (polkit-directory packages)
  "Return a directory containing an @file{actions} and possibly a
@file{rules.d} sub-directory, for use as @file{/etc/polkit-1}."
  (with-imported-modules '((guix build union))
    (computed-file "etc-polkit-1"
                   #~(begin
                       (use-modules (guix build union) (srfi srfi-26))

                       (union-build #$output
                                    (map (cut string-append <>
                                              "/share/polkit-1")
                                         (list #$@packages)))))))

(define polkit-etc-files
  (match-lambda
    (($ <polkit-configuration> polkit packages)
     `(("polkit-1" ,(polkit-directory (cons polkit packages)))))))

(define polkit-setuid-programs
  (match-lambda
    (($ <polkit-configuration> polkit)
     (list (file-append polkit "/lib/polkit-1/polkit-agent-helper-1")
           (file-append polkit "/bin/pkexec")))))

(define polkit-service-type
  (service-type (name 'polkit)
                (extensions
                 (list (service-extension account-service-type
                                          (const %polkit-accounts))
                       (service-extension pam-root-service-type
                                          (const %polkit-pam-services))
                       (service-extension dbus-root-service-type
                                          (compose
                                           list
                                           polkit-configuration-polkit))
                       (service-extension etc-service-type
                                          polkit-etc-files)
                       (service-extension setuid-program-service-type
                                          polkit-setuid-programs)))

                ;; Extensions are lists of packages that provide polkit rules
                ;; or actions under share/polkit-1/{actions,rules.d}.
                (compose concatenate)
                (extend (lambda (config actions)
                          (polkit-configuration
                           (inherit config)
                           (actions
                            (append (polkit-configuration-actions config)
                                    actions)))))

                (default-value (polkit-configuration))))

(define* (polkit-service #:key (polkit polkit))
  "Return a service that runs the
@uref{http://www.freedesktop.org/wiki/Software/polkit/, Polkit privilege
management service}, which allows system administrators to grant access to
privileged operations in a structured way.  By querying the Polkit service, a
privileged system component can know when it should grant additional
capabilities to ordinary users.  For example, an ordinary user can be granted
the capability to suspend the system if the user is logged in locally."
  (service polkit-service-type
           (polkit-configuration (polkit polkit))))

;;; dbus.scm ends here
