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
  #:use-module (gnu machine)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix remote)
  #:use-module (guix scripts system reconfigure)
  #:use-module (guix ssh)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
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

(define (deploy-managed-host machine)
  "Internal implementation of 'deploy-machine' for MACHINE instances with an
environment type of 'managed-host."
  (maybe-raise-unsupported-configuration-error machine)
  (mlet %store-monad ((boot-parameters (machine-boot-parameters machine)))
    (let* ((os (machine-system machine))
           (eval (cut machine-remote-eval machine <>))
           (menu-entries (map boot-parameters->menu-entry boot-parameters))
           (bootloader-configuration (operating-system-bootloader os))
           (bootcfg (operating-system-bootcfg os menu-entries)))
      (mbegin %store-monad
        (switch-to-system eval os)
        (upgrade-shepherd-services eval os)
        (install-bootloader eval bootloader-configuration bootcfg)))))


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
