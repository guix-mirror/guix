;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
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
  #:use-module (gnu services dmd)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services avahi)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu system shadow)
  #:use-module (gnu system linux) ; unix-pam-service
  #:use-module (gnu packages glib)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages polkit)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (upower-service
            colord-service
            geoclue-application
            %standard-geoclue-applications
            geoclue-service
            polkit-service
            elogind-configuration
            elogind-service
            %desktop-services))

;;; Commentary:
;;;
;;; This module contains service definitions for a "desktop" environment.
;;;
;;; Code:


;;;
;;; Helpers.
;;;

(define (bool value)
  (if value "true\n" "false\n"))


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

  (computed-file (string-append (package-name service) "-wrapper")
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
                               (find-files #$output "\\.service$")))
                 #:modules '((guix build utils))))


;;;
;;; Upower D-Bus service.
;;;

;; TODO: Export.
(define-record-type* <upower-configuration>
  upower-configuration make-upower-configuration
  upower-configuration?
  (upower        upower-configuration-upower
                 (default upower))
  (watts-up-pro? upower-configuration-watts-up-pro?)
  (poll-batteries? upower-configuration-poll-batteries?)
  (ignore-lid? upower-configuration-ignore-lid?)
  (use-percentage-for-policy? upower-configuration-use-percentage-for-policy?)
  (percentage-low upower-configuration-percentage-low)
  (percentage-critical upower-configuration-percentage-critical)
  (percentage-action upower-configuration-percentage-action)
  (time-low upower-configuration-time-low)
  (time-critical upower-configuration-time-critical)
  (time-action upower-configuration-time-action)
  (critical-power-action upower-configuration-critical-power-action))

(define* upower-configuration-file
  ;; Return an upower-daemon configuration file.
  (match-lambda
    (($ <upower-configuration> upower
        watts-up-pro? poll-batteries? ignore-lid? use-percentage-for-policy?
        percentage-low percentage-critical percentage-action time-low
        time-critical time-action critical-power-action)
     (plain-file "UPower.conf"
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
                  "\n")))))

(define %upower-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/upower")))

(define (upower-dbus-service config)
  (list (wrapped-dbus-service (upower-configuration-upower config)
                              "libexec/upowerd"
                              "UPOWER_CONF_FILE_NAME"
                              (upower-configuration-file config))))

(define (upower-dmd-service config)
  "Return a dmd service for UPower with CONFIG."
  (let ((upower (upower-configuration-upower config))
        (config (upower-configuration-file config)))
    (list (dmd-service
           (documentation "Run the UPower power and battery monitor.")
           (provision '(upower-daemon))
           (requirement '(dbus-system udev))

           (start #~(make-forkexec-constructor
                     (list (string-append #$upower "/libexec/upowerd"))
                     #:environment-variables
                     (list (string-append "UPOWER_CONF_FILE_NAME="
                                          #$config))))
           (stop #~(make-kill-destructor))))))

(define upower-service-type
  (service-type (name 'upower)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          upower-dbus-service)
                       (service-extension dmd-root-service-type
                                          upower-dmd-service)
                       (service-extension activation-service-type
                                          (const %upower-activation))
                       (service-extension udev-service-type
                                          (compose
                                           list
                                           upower-configuration-upower))))))

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
  (let ((config (upower-configuration
                 (watts-up-pro? watts-up-pro?)
                 (poll-batteries? poll-batteries?)
                 (ignore-lid? ignore-lid?)
                 (use-percentage-for-policy? use-percentage-for-policy?)
                 (percentage-low percentage-low)
                 (percentage-critical percentage-critical)
                 (percentage-action percentage-action)
                 (time-low time-low)
                 (time-critical time-critical)
                 (time-action time-action)
                 (critical-power-action critical-power-action))))
    (service upower-service-type config)))


;;;
;;; Colord D-Bus service.
;;;

(define %colord-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/colord")
      (let ((user (getpwnam "colord")))
        (chown "/var/lib/colord"
               (passwd:uid user) (passwd:gid user)))))

(define %colord-accounts
  (list (user-group (name "colord") (system? #t))
        (user-account
         (name "colord")
         (group "colord")
         (system? #t)
         (comment "colord daemon user")
         (home-directory "/var/empty")
         (shell #~(string-append #$shadow "/sbin/nologin")))))

(define colord-service-type
  (service-type (name 'colord)
                (extensions
                 (list (service-extension account-service-type
                                          (const %colord-accounts))
                       (service-extension activation-service-type
                                          (const %colord-activation))

                       ;; Colord is a D-Bus service that dbus-daemon can
                       ;; activate.
                       (service-extension dbus-root-service-type list)

                       ;; Colord provides "color device" rules for udev.
                       (service-extension udev-service-type list)))))

(define* (colord-service #:key (colord colord))
  "Return a service that runs @command{colord}, a system service with a D-Bus
interface to manage the color profiles of input and output devices such as
screens and scanners.  It is notably used by the GNOME Color Manager graphical
tool.  See @uref{http://www.freedesktop.org/software/colord/, the colord web
site} for more information."
  (service colord-service-type colord))


;;;
;;; GeoClue D-Bus service.
;;;

;; TODO: Export.
(define-record-type* <geoclue-configuration>
  geoclue-configuration make-geoclue-configuration
  geoclue-configuration?
  (geoclue geoclue-configuration-geoclue
           (default geoclue))
  (whitelist geoclue-configuration-whitelist)
  (wifi-geolocation-url geoclue-configuration-wifi-geolocation-url)
  (submit-data? geoclue-configuration-submit-data?)
  (wifi-submission-url geoclue-configuration-wifi-submission-url)
  (submission-nick geoclue-configuration-submission-nick)
  (applications geoclue-configuration-applications))

(define* (geoclue-application name #:key (allowed? #t) system? (users '()))
  "Configure default GeoClue access permissions for an application.  NAME is
the Desktop ID of the application, without the .desktop part.  If ALLOWED? is
true, the application will have access to location information by default.
The boolean SYSTEM? value indicates that an application is a system component
or not.  Finally USERS is a list of UIDs of all users for which this
application is allowed location info access.  An empty users list means all
users are allowed."
  (string-append
   "[" name "]\n"
   "allowed=" (bool allowed?)
   "system=" (bool system?)
   "users=" (string-join users ";") "\n"))

(define %standard-geoclue-applications
  (list (geoclue-application "gnome-datetime-panel" #:system? #t)
        (geoclue-application "epiphany" #:system? #f)
        (geoclue-application "firefox" #:system? #f)))

(define* (geoclue-configuration-file config)
  "Return a geoclue configuration file."
  (plain-file "geoclue.conf"
              (string-append
               "[agent]\n"
               "whitelist="
               (string-join (geoclue-configuration-whitelist config)
                            ";") "\n"
               "[wifi]\n"
               "url=" (geoclue-configuration-wifi-geolocation-url config) "\n"
               "submit-data=" (bool (geoclue-configuration-submit-data? config))
               "submission-url="
               (geoclue-configuration-wifi-submission-url config) "\n"
               "submission-nick="
               (geoclue-configuration-submission-nick config)
               "\n"
               (string-join (geoclue-configuration-applications config)
                            "\n"))))

(define (geoclue-dbus-service config)
  (list (wrapped-dbus-service (geoclue-configuration-geoclue config)
                              "libexec/geoclue"
                              "GEOCLUE_CONFIG_FILE"
                              (geoclue-configuration-file config))))

(define %geoclue-accounts
  (list (user-group (name "geoclue") (system? #t))
        (user-account
         (name "geoclue")
         (group "geoclue")
         (system? #t)
         (comment "GeoClue daemon user")
         (home-directory "/var/empty")
         (shell "/run/current-system/profile/sbin/nologin"))))

(define geoclue-service-type
  (service-type (name 'geoclue)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          geoclue-dbus-service)
                       (service-extension account-service-type
                                          (const %geoclue-accounts))))))

(define* (geoclue-service #:key (geoclue geoclue)
                          (whitelist '())
                          (wifi-geolocation-url
                           ;; Mozilla geolocation service:
                           "https://location.services.mozilla.com/v1/geolocate?key=geoclue")
                          (submit-data? #f)
                          (wifi-submission-url
                           "https://location.services.mozilla.com/v1/submit?key=geoclue")
                          (submission-nick "geoclue")
                          (applications %standard-geoclue-applications))
  "Return a service that runs the @command{geoclue} location service.  This
service provides a D-Bus interface to allow applications to request access to
a user's physical location, and optionally to add information to online
location databases.  By default, only the GNOME date-time panel and the Icecat
and Epiphany web browsers are able to ask for the user's location, and in the
case of Icecat and Epiphany, both will ask the user for permission first.  See
@uref{https://wiki.freedesktop.org/www/Software/GeoClue/, the geoclue web
site} for more information."
  (service geoclue-service-type
           (geoclue-configuration
            (geoclue geoclue)
            (whitelist whitelist)
            (wifi-geolocation-url wifi-geolocation-url)
            (submit-data? submit-data?)
            (wifi-submission-url wifi-submission-url)
            (submission-nick submission-nick)
            (applications applications))))


;;;
;;; Polkit privilege management service.
;;;

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
  (list (unix-pam-service "polkitd")))

(define polkit-service-type
  ;; TODO: Make it extensible so it can collect policy files from other
  ;; services.
  (service-type (name 'polkit)
                (extensions
                 (list (service-extension account-service-type
                                          (const %polkit-accounts))
                       (service-extension pam-root-service-type
                                          (const %polkit-pam-services))
                       (service-extension dbus-root-service-type
                                          list)))))

(define* (polkit-service #:key (polkit polkit))
  "Return a service that runs the @command{polkit} privilege management
service.  By querying the @command{polkit} service, a privileged system
component can know when it should grant additional capabilities to ordinary
users.  For example, an ordinary user can be granted the capability to suspend
the system if the user is logged in locally."
  (service polkit-service-type polkit))


;;;
;;; Elogind login and seat management service.
;;;

(define-record-type* <elogind-configuration> elogind-configuration
  make-elogind-configuration
  elogind-configuration
  (elogind                         elogind-package
                                   (default elogind))
  (kill-user-processes?            elogind-kill-user-processes?
                                   (default #f))
  (kill-only-users                 elogind-kill-only-users
                                   (default '()))
  (kill-exclude-users              elogind-kill-exclude-users
                                   (default '("root")))
  (inhibit-delay-max-seconds       elogind-inhibit-delay-max-seconds
                                   (default 5))
  (handle-power-key                elogind-handle-power-key
                                   (default 'poweroff))
  (handle-suspend-key              elogind-handle-suspend-key
                                   (default 'suspend))
  (handle-hibernate-key            elogind-handle-hibernate-key
                                   ;; (default 'hibernate)
                                   ;; XXX Ignore it for now, since we don't
                                   ;; yet handle resume-from-hibernation in
                                   ;; our initrd.
                                   (default 'ignore))
  (handle-lid-switch               elogind-handle-lid-switch
                                   (default 'suspend))
  (handle-lid-switch-docked        elogind-handle-lid-switch-docked
                                   (default 'ignore))
  (power-key-ignore-inhibited?     elogind-power-key-ignore-inhibited?
                                   (default #f))
  (suspend-key-ignore-inhibited?   elogind-suspend-key-ignore-inhibited?
                                   (default #f))
  (hibernate-key-ignore-inhibited? elogind-hibernate-key-ignore-inhibited?
                                   (default #f))
  (lid-switch-ignore-inhibited?    elogind-lid-switch-ignore-inhibited?
                                   (default #t))
  (holdoff-timeout-seconds         elogind-holdoff-timeout-seconds
                                   (default 30))
  (idle-action                     elogind-idle-action
                                   (default 'ignore))
  (idle-action-seconds             elogind-idle-action-seconds
                                   (default (* 30 60)))
  (runtime-directory-size-percent  elogind-runtime-directory-size-percent
                                   (default 10))
  (runtime-directory-size          elogind-runtime-directory-size
                                   (default #f))
  (remove-ipc?                     elogind-remove-ipc?
                                   (default #t))

  (suspend-state                   elogind-suspend-state
                                   (default '("mem" "standby" "freeze")))
  (suspend-mode                    elogind-suspend-mode
                                   (default '()))
  (hibernate-state                 elogind-hibernate-state
                                   (default '("disk")))
  (hibernate-mode                  elogind-hibernate-mode
                                   (default '("platform" "shutdown")))
  (hybrid-sleep-state              elogind-hybrid-sleep-state
                                   (default '("disk")))
  (hybrid-sleep-mode               elogind-hybrid-sleep-mode
                                   (default
                                     '("suspend" "platform" "shutdown"))))

(define (elogind-configuration-file config)
  (define (yesno x)
    (match x
      (#t "yes")
      (#f "no")
      (_ (error "expected #t or #f, instead got:" x))))
  (define char-set:user-name
    (string->char-set "abcdefghijklmnopqrstuvwxyz0123456789_-"))
  (define (valid-list? l pred)
    (and-map (lambda (x) (string-every pred x)) l))
  (define (user-name-list users)
    (unless (valid-list? users char-set:user-name)
      (error "invalid user list" users))
    (string-join users " "))
  (define (enum val allowed)
    (unless (memq val allowed)
      (error "invalid value" val allowed))
    (symbol->string val))
  (define (non-negative-integer x)
    (unless (exact-integer? x) (error "not an integer" x))
    (when (negative? x) (error "negative number not allowed" x))
    (number->string x))
  (define handle-actions
    '(ignore poweroff reboot halt kexec suspend hibernate hybrid-sleep lock))
  (define (handle-action x)
    (enum x handle-actions))
  (define (sleep-list tokens)
    (unless (valid-list? tokens char-set:user-name)
      (error "invalid sleep list" tokens))
    (string-join tokens " "))
  (define-syntax ini-file-clause
    (syntax-rules ()
      ((_ config (prop (parser getter)))
       (string-append prop "=" (parser (getter config)) "\n"))
      ((_ config str)
       (string-append str "\n"))))
  (define-syntax-rule (ini-file config file clause ...)
    (plain-file file (string-append (ini-file-clause config clause) ...)))
  (ini-file
   config "logind.conf"
   "[Login]"
   ("KillUserProcesses" (yesno elogind-kill-user-processes?))
   ("KillOnlyUsers" (user-name-list elogind-kill-only-users))
   ("KillExcludeUsers" (user-name-list elogind-kill-exclude-users))
   ("InhibitDelayMaxSecs" (non-negative-integer elogind-inhibit-delay-max-seconds))
   ("HandlePowerKey" (handle-action elogind-handle-power-key))
   ("HandleSuspendKey" (handle-action elogind-handle-suspend-key))
   ("HandleHibernateKey" (handle-action elogind-handle-hibernate-key))
   ("HandleLidSwitch" (handle-action elogind-handle-lid-switch))
   ("HandleLidSwitchDocked" (handle-action elogind-handle-lid-switch-docked))
   ("PowerKeyIgnoreInhibited" (yesno elogind-power-key-ignore-inhibited?))
   ("SuspendKeyIgnoreInhibited" (yesno elogind-suspend-key-ignore-inhibited?))
   ("HibernateKeyIgnoreInhibited" (yesno elogind-hibernate-key-ignore-inhibited?))
   ("LidSwitchIgnoreInhibited" (yesno elogind-lid-switch-ignore-inhibited?))
   ("HoldoffTimeoutSecs" (non-negative-integer elogind-holdoff-timeout-seconds))
   ("IdleAction" (handle-action elogind-idle-action))
   ("IdleActionSeconds" (non-negative-integer elogind-idle-action-seconds))
   ("RuntimeDirectorySize"
    (identity
     (lambda (config)
       (match (elogind-runtime-directory-size-percent config)
         (#f (non-negative-integer (elogind-runtime-directory-size config)))
         (percent (string-append (non-negative-integer percent) "%"))))))
   ("RemoveIpc" (yesno elogind-remove-ipc?))
   "[Sleep]"
   ("SuspendState" (sleep-list elogind-suspend-state))
   ("SuspendMode" (sleep-list elogind-suspend-mode))
   ("HibernateState" (sleep-list elogind-hibernate-state))
   ("HibernateMode" (sleep-list elogind-hibernate-mode))
   ("HybridSleepState" (sleep-list elogind-hybrid-sleep-state))
   ("HybridSleepMode" (sleep-list elogind-hybrid-sleep-mode))))

(define (elogind-dmd-service config)
  "Return a dmd service for elogind, using @var{config}."
  ;; TODO: We could probably rely on service activation but the '.service'
  ;; file currently contains an erroneous 'Exec' line.
  (let ((config-file (elogind-configuration-file config))
        (elogind     (elogind-package config)))
    (list (dmd-service
           (documentation "Run the elogind login and seat management service.")
           (provision '(elogind))
           (requirement '(dbus-system))

           (start #~(make-forkexec-constructor
                     (list (string-append #$elogind "/libexec/elogind/elogind"))
                     #:environment-variables
                     (list (string-append "ELOGIND_CONF_FILE=" #$config-file))))
           (stop #~(make-kill-destructor))))))

(define elogind-service-type
  (service-type (name 'elogind)
                (extensions
                 (list (service-extension dmd-root-service-type
                                          elogind-dmd-service)
                       (service-extension dbus-root-service-type
                                          (compose list elogind-package))
                       (service-extension udev-service-type
                                          (compose list elogind-package))
                       ;; TODO: Extend polkit(?) and PAM.
                       ))))

(define* (elogind-service #:key (config (elogind-configuration)))
  "Return a service that runs the @command{elogind} login and seat management
service.  The @command{elogind} service integrates with PAM to allow other
system components to know the set of logged-in users as well as their session
types (graphical, console, remote, etc.).  It can also clean up after users
when they log out."
  (service elogind-service-type config))


;;;
;;; The default set of desktop services.
;;;

(define %desktop-services
  ;; List of services typically useful for a "desktop" use case.
  (cons* (slim-service)

         ;; The D-Bus clique.
         (avahi-service)
         (wicd-service)
         (upower-service)
         (colord-service)
         (geoclue-service)
         (polkit-service)
         (elogind-service)
         (dbus-service)

         (ntp-service)

         %base-services))

;;; desktop.scm ends here
