;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.lonestar.org>
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

(define-module (gnu machine ssh)
  #:use-module (gnu bootloader)
  #:use-module (gnu machine)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix remote)
  #:use-module (guix ssh)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-35)
  #:export (managed-host-environment-type

            machine-ssh-configuration
            machine-ssh-configuration?
            machine-ssh-configuration

            machine-ssh-configuration-host-name
            machine-ssh-configuration-port
            machine-ssh-configuration-user
            machine-ssh-configuration-session))

;;; Commentary:
;;;
;;; This module implements remote evaluation and system deployment for
;;; machines that are accessible over SSH and have a known host-name. In the
;;; sense of the broader "machine" interface, we describe the environment for
;;; such machines as 'managed-host.
;;;
;;; Code:


;;;
;;; Parameters for the SSH client.
;;;

(define-record-type* <machine-ssh-configuration> machine-ssh-configuration
  make-machine-ssh-configuration
  machine-ssh-configuration?
  this-machine-ssh-configuration
  (host-name machine-ssh-configuration-host-name) ; string
  (port      machine-ssh-configuration-port       ; integer
             (default 22))
  (user      machine-ssh-configuration-user       ; string
             (default "root"))
  (identity  machine-ssh-configuration-identity   ; path to a private key
             (default #f))
  (session   machine-ssh-configuration-session    ; session
             (default #f)))

(define (machine-ssh-session machine)
  "Return the SSH session that was given in MACHINE's configuration, or create
one from the configuration's parameters if one was not provided."
  (maybe-raise-unsupported-configuration-error machine)
  (let ((config (machine-configuration machine)))
    (or (machine-ssh-configuration-session config)
        (let ((host-name (machine-ssh-configuration-host-name config))
              (user (machine-ssh-configuration-user config))
              (port (machine-ssh-configuration-port config))
              (identity (machine-ssh-configuration-identity config)))
          (open-ssh-session host-name
                            #:user user
                            #:port port
                            #:identity identity)))))


;;;
;;; Remote evaluation.
;;;

(define (managed-host-remote-eval machine exp)
  "Internal implementation of 'machine-remote-eval' for MACHINE instances with
an environment type of 'managed-host."
  (maybe-raise-unsupported-configuration-error machine)
  (remote-eval exp (machine-ssh-session machine)))


;;;
;;; System deployment.
;;;

(define (switch-to-system machine)
  "Monadic procedure creating a new generation on MACHINE and execute the
activation script for the new system configuration."
  (define (remote-exp drv script)
    (with-extensions (list guile-gcrypt)
      (with-imported-modules (source-module-closure '((guix config)
                                                      (guix profiles)
                                                      (guix utils)))
        #~(begin
            (use-modules (guix config)
                         (guix profiles)
                         (guix utils))

            (define %system-profile
              (string-append %state-directory "/profiles/system"))

            (let* ((system #$drv)
                   (number (1+ (generation-number %system-profile)))
                   (generation (generation-file-name %system-profile number)))
              (switch-symlinks generation system)
              (switch-symlinks %system-profile generation)
              ;; The implementation of 'guix system reconfigure' saves the
              ;; load path and environment here. This is unnecessary here
              ;; because each invocation of 'remote-eval' runs in a distinct
              ;; Guile REPL.
              (setenv "GUIX_NEW_SYSTEM" system)
              ;; The activation script may write to stdout, which confuses
              ;; 'remote-eval' when it attempts to read a result from the
              ;; remote REPL. We work around this by forcing the output to a
              ;; string.
              (with-output-to-string
                (lambda ()
                  (primitive-load #$script))))))))

  (let* ((os (machine-system machine))
         (script (operating-system-activation-script os)))
    (mlet* %store-monad ((drv (operating-system-derivation os)))
      (machine-remote-eval machine (remote-exp drv script)))))

;; XXX: Currently, this does NOT attempt to restart running services. This is
;; also the case with 'guix system reconfigure'.
;;
;; See <https://issues.guix.info/issue/33508>.
(define (upgrade-shepherd-services machine)
  "Monadic procedure unloading and starting services on the remote as needed
to realize the MACHINE's system configuration."
  (define target-services
    ;; Monadic expression evaluating to a list of (name output-path) pairs for
    ;; all of MACHINE's services.
    (mapm %store-monad
          (lambda (service)
            (mlet %store-monad ((file ((compose lower-object
                                                shepherd-service-file)
                                       service)))
              (return (list (shepherd-service-canonical-name service)
                            (derivation->output-path file)))))
          (service-value
           (fold-services (operating-system-services (machine-system machine))
                          #:target-type shepherd-root-service-type))))

  (define (remote-exp target-services)
    (with-imported-modules '((gnu services herd))
      #~(begin
          (use-modules (gnu services herd)
                       (srfi srfi-1))

          (define running
            (filter live-service-running (current-services)))

          (define (essential? service)
            ;; Return #t if SERVICE is essential and should not be unloaded
            ;; under any circumstance.
            (memq (first (live-service-provision service))
                  '(root shepherd)))

          (define (obsolete? service)
            ;; Return #t if SERVICE can be safely unloaded.
            (and (not (essential? service))
                 (every (lambda (requirements)
                          (not (memq (first (live-service-provision service))
                                     requirements)))
                        (map live-service-requirement running))))

          (define to-unload
            (filter obsolete?
                    (remove (lambda (service)
                              (memq (first (live-service-provision service))
                                    (map first '#$target-services)))
                            running)))

          (define to-start
            (remove (lambda (service-pair)
                      (memq (first service-pair)
                            (map (compose first live-service-provision)
                                 running)))
                    '#$target-services))

          ;; Unload obsolete services.
          (for-each (lambda (service)
                      (false-if-exception
                       (unload-service service)))
                    to-unload)

          ;; Load the service files for any new services and start them.
          (load-services/safe (map second to-start))
          (for-each start-service (map first to-start))

          #t)))

  (mlet %store-monad ((target-services target-services))
    (machine-remote-eval machine (remote-exp target-services))))

(define (machine-boot-parameters machine)
  "Monadic procedure returning a list of 'boot-parameters' for the generations
of MACHINE's system profile, ordered from most recent to oldest."
  (define bootable-kernel-arguments
    (@@ (gnu system) bootable-kernel-arguments))

  (define remote-exp
    (with-extensions (list guile-gcrypt)
      (with-imported-modules (source-module-closure '((guix config)
                                                      (guix profiles)))
        #~(begin
            (use-modules (guix config)
                         (guix profiles)
                         (ice-9 textual-ports))

            (define %system-profile
              (string-append %state-directory "/profiles/system"))

            (define (read-file path)
              (call-with-input-file path
                (lambda (port)
                  (get-string-all port))))

            (map (lambda (generation)
                   (let* ((system-path (generation-file-name %system-profile
                                                             generation))
                          (boot-parameters-path (string-append system-path
                                                               "/parameters"))
                          (time (stat:mtime (lstat system-path))))
                     (list generation
                           system-path
                           time
                           (read-file boot-parameters-path))))
                 (reverse (generation-numbers %system-profile)))))))

  (mlet* %store-monad ((generations (machine-remote-eval machine remote-exp)))
    (return
     (map (lambda (generation)
            (match generation
              ((generation system-path time serialized-params)
               (let* ((params (call-with-input-string serialized-params
                                read-boot-parameters))
                      (root (boot-parameters-root-device params))
                      (label (boot-parameters-label params)))
                 (boot-parameters
                  (inherit params)
                  (label
                   (string-append label " (#"
                                  (number->string generation) ", "
                                  (let ((time (make-time time-utc 0 time)))
                                    (date->string (time-utc->date time)
                                                  "~Y-~m-~d ~H:~M"))
                                  ")"))
                  (kernel-arguments
                   (append (bootable-kernel-arguments system-path root)
                           (boot-parameters-kernel-arguments params))))))))
          generations))))

(define (install-bootloader machine)
  "Create a bootloader entry for the new system generation on MACHINE, and
configure the bootloader to boot that generation by default."
  (define bootloader-installer-script
    (@@ (guix scripts system) bootloader-installer-script))

  (define (remote-exp installer bootcfg bootcfg-file)
    (with-extensions (list guile-gcrypt)
      (with-imported-modules (source-module-closure '((gnu build install)
                                                      (guix store)
                                                      (guix utils)))
        #~(begin
            (use-modules (gnu build install)
                         (guix store)
                         (guix utils))
            (let* ((gc-root (string-append "/" %gc-roots-directory "/bootcfg"))
                   (temp-gc-root (string-append gc-root ".new")))

              (switch-symlinks temp-gc-root gc-root)

              (unless (false-if-exception
                       (begin
                         ;; The implementation of 'guix system reconfigure'
                         ;; saves the load path here. This is unnecessary here
                         ;; because each invocation of 'remote-eval' runs in a
                         ;; distinct Guile REPL.
                         (install-boot-config #$bootcfg #$bootcfg-file "/")
                         ;; The installation script may write to stdout, which
                         ;; confuses 'remote-eval' when it attempts to read a
                         ;; result from the remote REPL. We work around this
                         ;; by forcing the output to a string.
                         (with-output-to-string
                           (lambda ()
                             (primitive-load #$installer)))))
                (delete-file temp-gc-root)
                (error "failed to install bootloader"))

              (rename-file temp-gc-root gc-root)
              #t)))))

  (mlet* %store-monad ((boot-parameters (machine-boot-parameters machine)))
    (let* ((os (machine-system machine))
           (bootloader ((compose bootloader-configuration-bootloader
                                 operating-system-bootloader)
                        os))
           (bootloader-target (bootloader-configuration-target
                               (operating-system-bootloader os)))
           (installer (bootloader-installer-script
                       (bootloader-installer bootloader)
                       (bootloader-package bootloader)
                       bootloader-target
                       "/"))
           (menu-entries (map boot-parameters->menu-entry boot-parameters))
           (bootcfg (operating-system-bootcfg os menu-entries))
           (bootcfg-file (bootloader-configuration-file bootloader)))
      (machine-remote-eval machine (remote-exp installer bootcfg bootcfg-file)))))

(define (deploy-managed-host machine)
  "Internal implementation of 'deploy-machine' for MACHINE instances with an
environment type of 'managed-host."
  (maybe-raise-unsupported-configuration-error machine)
  (mbegin %store-monad
    (switch-to-system machine)
    (upgrade-shepherd-services machine)
    (install-bootloader machine)))


;;;
;;; Environment type.
;;;

(define managed-host-environment-type
  (environment-type
   (machine-remote-eval managed-host-remote-eval)
   (deploy-machine      deploy-managed-host)
   (name                'managed-host-environment-type)
   (description         "Provisioning for machines that are accessible over SSH
and have a known host-name. This entails little more than maintaining an SSH
connection to the host.")))

(define (maybe-raise-unsupported-configuration-error machine)
  "Raise an error if MACHINE's configuration is not an instance of
<machine-ssh-configuration>."
  (let ((config (machine-configuration machine))
        (environment (environment-type-name (machine-environment machine))))
    (unless (and config (machine-ssh-configuration? config))
      (raise (condition
              (&message
               (message (format #f (G_ "unsupported machine configuration '~a'
for environment of type '~a'")
                                config
                                environment))))))))
