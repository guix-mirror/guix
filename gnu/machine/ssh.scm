;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
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
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system uuid)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix pki)
  #:use-module (guix records)
  #:use-module (guix remote)
  #:use-module (guix scripts system reconfigure)
  #:use-module (guix ssh)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (gcrypt pk-crypto)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (managed-host-environment-type

            machine-ssh-configuration
            machine-ssh-configuration?
            machine-ssh-configuration

            machine-ssh-configuration-host-name
            machine-ssh-configuration-build-locally?
            machine-ssh-configuration-authorize?
            machine-ssh-configuration-port
            machine-ssh-configuration-user
            machine-ssh-configuration-host-key
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
  (host-name      machine-ssh-configuration-host-name)     ; string
  (system         machine-ssh-configuration-system)        ; string
  (build-locally? machine-ssh-configuration-build-locally? ; boolean
                  (default #t))
  (authorize?     machine-ssh-configuration-authorize?     ; boolean
                  (default #t))
  (port           machine-ssh-configuration-port           ; integer
                  (default 22))
  (user           machine-ssh-configuration-user           ; string
                  (default "root"))
  (identity       machine-ssh-configuration-identity       ; path to a private key
                  (default #f))
  (session        machine-ssh-configuration-session        ; session
                  (default #f))
  (host-key       machine-ssh-configuration-host-key       ; #f | string
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
              (identity (machine-ssh-configuration-identity config))
              (host-key (machine-ssh-configuration-host-key config)))
          (unless host-key
            (warning (G_ "<machine-ssh-configuration> without a 'host-key' \
is deprecated~%")))
          (open-ssh-session host-name
                            #:user user
                            #:port port
                            #:identity identity
                            #:host-key host-key)))))


;;;
;;; Remote evaluation.
;;;

(define (machine-become-command machine)
  "Return as a list of strings the program and arguments necessary to run a
shell command with escalated privileges for MACHINE's configuration."
  (if (string= "root" (machine-ssh-configuration-user
                       (machine-configuration machine)))
      '()
      '("/run/setuid-programs/sudo" "-n" "--")))

(define (managed-host-remote-eval machine exp)
  "Internal implementation of 'machine-remote-eval' for MACHINE instances with
an environment type of 'managed-host."
  (maybe-raise-unsupported-configuration-error machine)
  (let ((config (machine-configuration machine)))
    (remote-eval exp (machine-ssh-session machine)
                 #:build-locally?
                 (machine-ssh-configuration-build-locally? config)
                 #:system
                 (machine-ssh-configuration-system config)
                 #:become-command
                 (machine-become-command machine))))


;;;
;;; Safety checks.
;;;

(define (machine-check-file-system-availability machine)
  "Raise a '&message' error condition if any of the file-systems specified in
MACHINE's 'system' declaration do not exist on the machine."
  (define file-systems
    (filter (lambda (fs)
              (and (file-system-mount? fs)
                   (not (member (file-system-type fs)
                                %pseudo-file-system-types))
                   (not (memq 'bind-mount (file-system-flags fs)))))
            (operating-system-file-systems (machine-operating-system machine))))

  (define (check-literal-file-system fs)
    (define remote-exp
      #~(catch 'system-error
          (lambda ()
            (stat #$(file-system-device fs))
            #t)
          (lambda args
            (system-error-errno args))))

    (mlet %store-monad ((errno (machine-remote-eval machine remote-exp)))
      (when (number? errno)
        (raise (condition
                (&message
                 (message (format #f (G_ "device '~a' not found: ~a")
                                  (file-system-device fs)
                                  (strerror errno)))))))
      (return #t)))

  (define (check-labeled-file-system fs)
    (define remote-exp
      (with-imported-modules (source-module-closure
                              '((gnu build file-systems)))
        #~(begin
            (use-modules (gnu build file-systems))
            (find-partition-by-label #$(file-system-label->string
                                        (file-system-device fs))))))

    (mlet %store-monad ((result (machine-remote-eval machine remote-exp)))
      (unless result
        (raise (condition
                (&message
                 (message (format #f (G_ "no file system with label '~a'")
                                  (file-system-label->string
                                   (file-system-device fs))))))))
      (return #t)))

  (define (check-uuid-file-system fs)
    (define remote-exp
      (with-imported-modules (source-module-closure
                              '((gnu build file-systems)
                                (gnu system uuid)))
        #~(begin
            (use-modules (gnu build file-systems)
                         (gnu system uuid))

            (define uuid
              (string->uuid #$(uuid->string (file-system-device fs))))

            (find-partition-by-uuid uuid))))

    (mlet %store-monad ((result (machine-remote-eval machine remote-exp)))
      (unless result
        (raise (condition
                (&message
                 (message (format #f (G_ "no file system with UUID '~a'")
                                  (uuid->string (file-system-device fs))))))))
      (return #t)))

  (mbegin %store-monad
    (mapm %store-monad check-literal-file-system
          (filter (lambda (fs)
                    (string? (file-system-device fs)))
                  file-systems))
    (mapm %store-monad check-labeled-file-system
          (filter (lambda (fs)
                    (file-system-label? (file-system-device fs)))
                  file-systems))
    (mapm %store-monad check-uuid-file-system
          (filter (lambda (fs)
              (uuid? (file-system-device fs)))
                  file-systems))))

(define (machine-check-initrd-modules machine)
  "Raise a '&message' error condition if any of the modules needed by
'needed-for-boot' file systems in MACHINE are not available in the initrd."
  (define file-systems
    (filter file-system-needed-for-boot?
            (operating-system-file-systems (machine-operating-system machine))))

  (define (missing-modules fs)
    (define remote-exp
      (let ((device (file-system-device fs)))
        (with-imported-modules (source-module-closure
                                '((gnu build file-systems)
                                  (gnu build linux-modules)
                                  (gnu system uuid)))
          #~(begin
              (use-modules (gnu build file-systems)
                           (gnu build linux-modules)
                           (gnu system uuid))

              (define dev
                #$(cond ((string? device) device)
                        ((uuid? device) #~(find-partition-by-uuid
                                           (string->uuid
                                            #$(uuid->string device))))
                        ((file-system-label? device)
                         #~(find-partition-by-label
                            #$(file-system-label->string device)))))

              (missing-modules dev '#$(operating-system-initrd-modules
                                       (machine-operating-system machine)))))))
    (mlet %store-monad ((missing (machine-remote-eval machine remote-exp)))
      (return (list fs missing))))

  (mlet %store-monad ((device (mapm %store-monad missing-modules file-systems)))
    (for-each (match-lambda
                ((fs missing)
                 (unless (null? missing)
                   (raise (condition
                           (&message
                            (message (format #f (G_ "~a missing modules ~{ ~a~}~%")
                                             (file-system-device fs)
                                             missing))))))))
              device)
    (return #t)))

(define (machine-check-building-for-appropriate-system machine)
  "Raise a '&message' error condition if MACHINE is configured to be built
locally and the 'system' field does not match the '%current-system' reported
by MACHINE."
  (let ((config (machine-configuration machine))
        (system (remote-system (machine-ssh-session machine))))
    (when (and (machine-ssh-configuration-build-locally? config)
               (not (string= system (machine-ssh-configuration-system config))))
      (raise (condition
              (&message
               (message (format #f (G_ "incorrect target system \
('~a' was given, while the system reports that it is '~a')~%")
                                (machine-ssh-configuration-system config)
                                system)))))))
  (with-monad %store-monad (return #t)))

(define (check-deployment-sanity machine)
  "Raise a '&message' error condition if it is clear that deploying MACHINE's
'system' declaration would fail."
  ;; Order is important here -- an incorrect value for 'system' will cause
  ;; invocations of 'remote-eval' to fail.
  (mbegin %store-monad
    (machine-check-building-for-appropriate-system machine)
    (machine-check-file-system-availability machine)
    (machine-check-initrd-modules machine)))


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

(define-syntax-rule (with-roll-back should-roll-back? mbody ...)
  "Catch exceptions that arise when binding MBODY, a monadic expression in
%STORE-MONAD, and collect their arguments in a &deploy-error condition, with
the 'should-roll-back' field set to SHOULD-ROLL-BACK?"
  (catch #t
    (lambda ()
      mbody ...)
    (lambda args
      (raise (condition (&deploy-error
                         (should-roll-back should-roll-back?)
                         (captured-args args)))))))

(define (deploy-managed-host machine)
  "Internal implementation of 'deploy-machine' for MACHINE instances with an
environment type of 'managed-host."
  (maybe-raise-unsupported-configuration-error machine)
  (when (machine-ssh-configuration-authorize?
         (machine-configuration machine))
    (unless (file-exists? %public-key-file)
      (raise (condition
              (&message
               (message (format #f (G_ "no signing key '~a'. \
have you run 'guix archive --generate-key?'")
                                %public-key-file))))))
    (remote-authorize-signing-key (call-with-input-file %public-key-file
                                    (lambda (port)
                                      (string->canonical-sexp
                                       (get-string-all port))))
                                  (machine-ssh-session machine)
                                  (machine-become-command machine)))
  (mlet %store-monad ((_ (check-deployment-sanity machine))
                      (boot-parameters (machine-boot-parameters machine)))
    (let* ((os (machine-operating-system machine))
           (eval (cut machine-remote-eval machine <>))
           (menu-entries (map boot-parameters->menu-entry boot-parameters))
           (bootloader-configuration (operating-system-bootloader os))
           (bootcfg (operating-system-bootcfg os menu-entries)))
      (mbegin %store-monad
        (with-roll-back #f
          (switch-to-system eval os))
        (with-roll-back #t
          (mbegin %store-monad
            (upgrade-shepherd-services eval os)
            (install-bootloader eval bootloader-configuration bootcfg)))))))


;;;
;;; Roll-back.
;;;

(define (roll-back-managed-host machine)
  "Internal implementation of 'roll-back-machine' for MACHINE instances with
an environment type of 'managed-host."
  (define remote-exp
    (with-extensions (list guile-gcrypt)
      (with-imported-modules (source-module-closure '((guix config)
                                                      (guix profiles)))
        #~(begin
            (use-modules (guix config)
                         (guix profiles))

            (define %system-profile
              (string-append %state-directory "/profiles/system"))

            (define target-generation
              (relative-generation %system-profile -1))

            (if target-generation
                (switch-to-generation %system-profile target-generation)
                'error)))))

  (define roll-back-failure
    (condition (&message (message (G_ "could not roll-back machine")))))

  (mlet* %store-monad ((boot-parameters (machine-boot-parameters machine))
                       (_ -> (if (< (length boot-parameters) 2)
                                 (raise roll-back-failure)))
                       (entries -> (map boot-parameters->menu-entry
                                        (list (second boot-parameters))))
                       (old-entries -> (map boot-parameters->menu-entry
                                            (drop boot-parameters 2)))
                       (bootloader -> (operating-system-bootloader
                                       (machine-operating-system machine)))
                       (bootcfg (lower-object
                                 ((bootloader-configuration-file-generator
                                   (bootloader-configuration-bootloader
                                    bootloader))
                                  bootloader entries
                                  #:old-entries old-entries)))
                       (remote-result (machine-remote-eval machine remote-exp)))
    (when (eqv? 'error remote-result)
      (raise roll-back-failure))))


;;;
;;; Environment type.
;;;

(define managed-host-environment-type
  (environment-type
   (machine-remote-eval managed-host-remote-eval)
   (deploy-machine      deploy-managed-host)
   (roll-back-machine   roll-back-managed-host)
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
