;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2020-2022 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((gnu services) #:select (sexp->system-provenance))
  #:use-module (guix diagnostics)
  #:use-module (guix memoization)
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
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module ((guix inferior)
                #:select (inferior-exception?
                          inferior-exception-arguments))
  #:use-module (gcrypt pk-crypto)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
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
            machine-ssh-configuration-allow-downgrades?
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
  this-machine-ssh-configuration
  (host-name      machine-ssh-configuration-host-name)     ; string
  (system         machine-ssh-configuration-system)        ; string
  (build-locally? machine-ssh-configuration-build-locally? ; boolean
                  (default #t))
  (authorize?     machine-ssh-configuration-authorize?     ; boolean
                  (default #t))
  (allow-downgrades? machine-ssh-configuration-allow-downgrades? ; boolean
                     (default #f))
  (safety-checks?    machine-ssh-configuration-safety-checks? ;boolean
                     (default #t))
  (port           machine-ssh-configuration-port           ; integer
                  (default 22))
  (user           machine-ssh-configuration-user           ; string
                  (default "root"))
  (identity       machine-ssh-configuration-identity       ; path to a private key
                  (default #f))
  (session        machine-ssh-configuration-session        ; session
                  (thunked)
                  (default
                    ;; By default, open the session once and cache it.
                    (open-machine-ssh-session* this-machine-ssh-configuration)))
  (host-key       machine-ssh-configuration-host-key       ; #f | string
                  (default #f)))

(define (open-machine-ssh-session config)
  "Open an SSH session for CONFIG, a <machine-ssh-configuration> record."
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
                      #:host-key host-key)))

(define open-machine-ssh-session*
  (mlambdaq (config)
    "Memoizing variant of 'open-machine-ssh-session'."
    (open-machine-ssh-session config)))

(define (machine-ssh-session machine)
  "Return the SSH session that was given in MACHINE's configuration, or create
one from the configuration's parameters if one was not provided."
  (maybe-raise-unsupported-configuration-error machine)
  (let ((config (machine-configuration machine)))
    (or (machine-ssh-configuration-session config)
        (open-machine-ssh-session config))))


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

;; Assertion to be executed remotely.  This abstraction exists to allow us to
;; gather a list of expressions to be evaluated and eventually evaluate them
;; all at once instead of one by one.  (This is pretty much a monad.)
(define-record-type <remote-assertion>
  (remote-assertion exp proc)
  remote-assertion?
  (exp   remote-assertion-expression)
  (proc  remote-assertion-procedure))

(define-syntax-rule (remote-let ((var exp)) body ...)
  "Return a <remote-assertion> that binds VAR to the result of evaluating EXP,
a gexp, remotely, and evaluate BODY in that context."
  (remote-assertion exp (lambda (var) body ...)))

(define (machine-check-file-system-availability machine)
  "Return a list of <remote-assertion> that raise a '&message' error condition
if any of the file-systems specified in MACHINE's 'system' declaration do not
exist on the machine."
  (define file-systems
    (filter (lambda (fs)
              (and (file-system-mount? fs)
                   (not (member (file-system-type fs)
                                %pseudo-file-system-types))
                   ;; Don't try to validate network file systems.
                   (not (string-prefix? "nfs" (file-system-type fs)))
                   (not (memq 'bind-mount (file-system-flags fs)))))
            (operating-system-file-systems (machine-operating-system machine))))

  (define (check-literal-file-system fs)
    (remote-let ((errno #~(catch 'system-error
                            (lambda ()
                              (stat #$(file-system-device fs))
                              #t)
                            (lambda args
                              (system-error-errno args)))))
      (when (number? errno)
        (raise (formatted-message (G_ "device '~a' not found: ~a")
                                  (file-system-device fs)
                                  (strerror errno))))))

  (define (check-labeled-file-system fs)
    (define remote-exp
      (with-imported-modules (source-module-closure
                              '((gnu build file-systems)))
        #~(begin
            (use-modules (gnu build file-systems))
            (find-partition-by-label #$(file-system-label->string
                                        (file-system-device fs))))))

    (remote-let ((result remote-exp))
      (unless result
        (raise (formatted-message (G_ "no file system with label '~a'")
                                  (file-system-label->string
                                   (file-system-device fs)))))))

  (define (check-uuid-file-system fs)
    (define remote-exp
      (with-imported-modules (source-module-closure
                              '((gnu build file-systems)
                                (gnu system uuid)))
        #~(begin
            (use-modules (gnu build file-systems)
                         (gnu system uuid))

            (let ((uuid (uuid #$(uuid->string (file-system-device fs))
                              '#$(uuid-type (file-system-device fs)))))
              (find-partition-by-uuid uuid)))))

    (remote-let ((result remote-exp))
      (unless result
        (raise (formatted-message (G_ "no file system with UUID '~a'")
                                  (uuid->string (file-system-device fs)))))))

  (if (machine-ssh-configuration-safety-checks?
       (machine-configuration machine))
      (append (map check-literal-file-system
                   (filter (lambda (fs)
                             (string? (file-system-device fs)))
                           file-systems))
              (map check-labeled-file-system
                   (filter (lambda (fs)
                             (file-system-label? (file-system-device fs)))
                           file-systems))
              (map check-uuid-file-system
                   (filter (lambda (fs)
                             (uuid? (file-system-device fs)))
                           file-systems)))
      '()))

(define (machine-check-initrd-modules machine)
  "Return a list of <remote-assertion> that raise a '&message' error condition
if any of the modules needed by 'needed-for-boot' file systems in MACHINE are
not available in the initrd."
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

    (remote-let ((missing remote-exp))
      (unless (null? missing)
        (raise (formatted-message (G_ "missing modules for ~a:~{ ~a~}~%")
                                  (file-system-device fs)
                                  missing)))))

  (if (machine-ssh-configuration-safety-checks?
       (machine-configuration machine))
      (map missing-modules file-systems)
      '()))

(define* (machine-check-forward-update machine)
  "Check whether we are making a forward update for MACHINE.  Depending on its
'allow-upgrades?' field, raise an error or display a warning if we are
potentially downgrading it."
  (define config
    (machine-configuration machine))

  (define validate-reconfigure
    (if (machine-ssh-configuration-allow-downgrades? config)
        warn-about-backward-reconfigure
        ensure-forward-reconfigure))

  (remote-let ((provenance #~(call-with-input-file
                                 "/run/current-system/provenance"
                               read)))
    (define channels
      (sexp->system-provenance provenance))

    (check-forward-update validate-reconfigure
                          #:current-channels channels)))

(define (machine-check-building-for-appropriate-system machine)
  "Raise a '&message' error condition if MACHINE is configured to be built
locally and the 'system' field does not match the '%current-system' reported
by MACHINE."
  (let ((config (machine-configuration machine))
        (system (remote-system (machine-ssh-session machine))))
    (when (and (machine-ssh-configuration-build-locally? config)
               (not (string= system (machine-ssh-configuration-system config))))
      (raise (formatted-message (G_ "incorrect target system\
 ('~a' was given, while the system reports that it is '~a')~%")
                                (machine-ssh-configuration-system config)
                                system)))))

(define (check-deployment-sanity machine)
  "Raise a '&message' error condition if it is clear that deploying MACHINE's
'system' declaration would fail."
  (define assertions
    (append (machine-check-file-system-availability machine)
            (machine-check-initrd-modules machine)
            (list (machine-check-forward-update machine))))

  (define aggregate-exp
    ;; Gather all the expressions so that a single round-trip is enough to
    ;; evaluate all the ASSERTIONS remotely.
    #~(map (lambda (file)
             (false-if-exception (primitive-load file)))
           '#$(map (lambda (assertion)
                     (scheme-file "remote-assertion.scm"
                                  (remote-assertion-expression assertion)))
                   assertions)))

  ;; First check MACHINE's system type--an incorrect value for 'system' would
  ;; cause subsequent invocations of 'remote-eval' to fail.
  (machine-check-building-for-appropriate-system machine)

  (mlet %store-monad ((values (machine-remote-eval machine aggregate-exp)))
    (for-each (lambda (proc value)
                (proc value))
              (map remote-assertion-procedure assertions)
              values)
    (return #t)))


;;;
;;; System deployment.
;;;

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (_ #f)))

(define (machine-boot-parameters machine)
  "Monadic procedure returning a list of 'boot-parameters' for the generations
of MACHINE's system profile, ordered from most recent to oldest."
  (define bootable-kernel-arguments
    (@@ (gnu system) bootable-kernel-arguments))

  (define remote-exp
    (with-extensions (list guile-gcrypt)
      (with-imported-modules `(((guix config) => ,(make-config.scm))
                               ,@(source-module-closure
                                  '((guix profiles))
                                  #:select? not-config?))
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
      (raise (formatted-message (G_ "no signing key '~a'. \
have you run 'guix archive --generate-key?'")
                                %public-key-file)))
    (remote-authorize-signing-key (call-with-input-file %public-key-file
                                    (lambda (port)
                                      (string->canonical-sexp
                                       (get-string-all port))))
                                  (machine-ssh-session machine)
                                  (machine-become-command machine)))
  (mlet %store-monad ((_ (check-deployment-sanity machine))
                      (boot-parameters (machine-boot-parameters machine)))
    (let* ((os (machine-operating-system machine))
           (host (machine-ssh-configuration-host-name
                  (machine-configuration machine)))
           (eval (cut machine-remote-eval machine <>))
           (menu-entries (map boot-parameters->menu-entry boot-parameters))
           (bootloader-configuration (operating-system-bootloader os))
           (bootcfg (operating-system-bootcfg os menu-entries)))
      (define-syntax-rule (eval/error-handling condition handler ...)
        ;; Return a wrapper around EVAL such that HANDLER is evaluated if an
        ;; exception is raised.
        (lambda (exp)
          (lambda (store)
            (guard (condition ((inferior-exception? condition)
                               (values (begin handler ...) store)))
              (values (run-with-store store (eval exp))
                      store)))))

      (mbegin %store-monad
        (with-roll-back #f
          (switch-to-system (eval/error-handling c
                              (raise (formatted-message
                                      (G_ "\
failed to switch systems while deploying '~a':~%~{~s ~}")
                                      host
                                      (inferior-exception-arguments c))))
                            os))
        (with-roll-back #t
          (mbegin %store-monad
            (upgrade-shepherd-services (eval/error-handling c
                                         (warning (G_ "\
an error occurred while upgrading services on '~a':~%~{~s ~}~%")
                                                  host
                                                  (inferior-exception-arguments
                                                   c)))
                                       os)
            (install-bootloader (eval/error-handling c
                                  (raise (formatted-message
                                          (G_ "\
failed to install bootloader on '~a':~%~{~s ~}~%")
                                          host
                                          (inferior-exception-arguments c))))
                                bootloader-configuration bootcfg)))))))


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
                       (locale -> (boot-parameters-locale
                                   (second boot-parameters)))
                       (crypto-dev -> (boot-parameters-store-crypto-devices
                                       (second boot-parameters)))
                       (store-dir -> (boot-parameters-store-directory-prefix
                                      (second boot-parameters)))
                       (old-entries -> (map boot-parameters->menu-entry
                                            (drop boot-parameters 2)))
                       (bootloader -> (operating-system-bootloader
                                       (machine-operating-system machine)))
                       (bootcfg (lower-object
                                 ((bootloader-configuration-file-generator
                                   (bootloader-configuration-bootloader
                                    bootloader))
                                  bootloader entries
                                  #:locale locale
                                  #:store-crypto-devices crypto-dev
                                  #:store-directory-prefix store-dir
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
      (raise (formatted-message (G_ "unsupported machine configuration '~a'
for environment of type '~a'")
                                config
                                environment)))))

;; Local Variables:
;; eval: (put 'remote-let 'scheme-indent-function 1)
;; eval: (put 'with-roll-back 'scheme-indent-function 1)
;; eval: (put 'eval/error-handling 'scheme-indent-function 1)
;; End:
