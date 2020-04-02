;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017, 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
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

(define-module (guix scripts system reconfigure)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu bootloader)
  #:use-module (gnu services)
  #:use-module (gnu services herd)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (switch-system-program
            switch-to-system

            upgrade-services-program
            upgrade-shepherd-services

            install-bootloader-program
            install-bootloader))

;;; Commentary:
;;;
;;; This module implements the "effectful" parts of system
;;; reconfiguration. Although building a system derivation is a pure
;;; operation, a number of impure operations must be carried out for the
;;; system configuration to be realized -- chiefly, creation of generation
;;; symlinks and invocation of activation scripts.
;;;
;;; Code:


;;;
;;; Profile creation.
;;;

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix rest ...) #t)
    (('gnu rest ...) #t)
    (_ #f)))

(define* (switch-system-program os #:optional profile)
  "Return an executable store item that, upon being evaluated, will create a
new generation of PROFILE pointing to the directory of OS, switch to it
atomically, and run OS's activation script."
  (program-file
   "switch-to-system.scm"
   (with-extensions (list guile-gcrypt)
     (with-imported-modules `(,@(source-module-closure
                                 '((guix profiles)
                                   (guix utils))
                                 #:select? not-config?)
                              ((guix config) => ,(make-config.scm)))
       #~(begin
           (use-modules (guix config)
                        (guix profiles)
                        (guix utils))

           (define profile
             (or #$profile (string-append %state-directory "/profiles/system")))

           (let* ((number (1+ (generation-number profile)))
                  (generation (generation-file-name profile number)))
             (switch-symlinks generation #$os)
             (switch-symlinks profile generation)
             (setenv "GUIX_NEW_SYSTEM" #$os)
             (primitive-load #$(operating-system-activation-script os))))))))

(define* (switch-to-system eval os #:optional profile)
  "Using EVAL, a monadic procedure taking a single G-Expression as an argument,
create a new generation of PROFILE pointing to the directory of OS, switch to
it atomically, and run OS's activation script."
  (eval #~(primitive-load #$(switch-system-program os profile))))


;;;
;;; Services.
;;;

(define (running-services eval)
  "Using EVAL, a monadic procedure taking a single G-Expression as an argument,
return the <live-service> objects that are currently running on MACHINE."
  (define exp
    (with-imported-modules '((gnu services herd))
      #~(begin
          (use-modules (gnu services herd))
          (let ((services (current-services)))
            (and services
                 ;; 'live-service-running' is ignored, as we can't necessarily
                 ;; serialize arbitrary objects. This should be fine for now,
                 ;; since 'machine-current-services' is not exposed publicly,
                 ;; and the resultant <live-service> objects are only used for
                 ;; resolving service dependencies.
                 (map (lambda (service)
                        (list (live-service-provision service)
                              (live-service-requirement service)))
                      services))))))
  (mlet %store-monad ((services (eval exp)))
    (return (map (match-lambda
                   ((provision requirement)
                    (live-service provision requirement #f)))
                 services))))

;; XXX: Currently, this does NOT attempt to restart running services. See
;; <https://issues.guix.info/issue/33508> for details.
(define (upgrade-services-program service-files to-start to-unload to-restart)
  "Return an executable store item that, upon being evaluated, will upgrade
the Shepherd (PID 1) by unloading obsolete services and loading new
services. SERVICE-FILES is a list of Shepherd service files to load, and
TO-START, TO-UNLOAD, and TO-RESTART are lists of the Shepherd services'
canonical names (symbols)."
  (program-file
   "upgrade-shepherd-services.scm"
   (with-imported-modules '((gnu services herd))
    #~(begin
        (use-modules (gnu services herd)
                     (srfi srfi-1))

        ;; Load the service files for any new services.
        ;; Silence messages coming from shepherd such as "Evaluating
        ;; expression ..." since they are unhelpful.
        (parameterize ((shepherd-message-port (%make-void-port "w")))
          (load-services/safe '#$service-files))

        ;; Unload obsolete services and start new services.
        (for-each unload-service '#$to-unload)
        (for-each start-service '#$to-start)))))

(define* (upgrade-shepherd-services eval os)
  "Using EVAL, a monadic procedure taking a single G-Expression as an argument,
upgrade the Shepherd (PID 1) by unloading obsolete services and loading new
services as defined by OS."
  (define target-services
    (service-value
     (fold-services (operating-system-services os)
                    #:target-type shepherd-root-service-type)))

  (mlet* %store-monad ((live-services (running-services eval)))
    (let*-values (((to-unload to-restart)
                   (shepherd-service-upgrade live-services target-services)))
      (let* ((to-unload (map live-service-canonical-name to-unload))
             (to-restart (map shepherd-service-canonical-name to-restart))
             (to-start (lset-difference eqv?
                                        (map shepherd-service-canonical-name
                                             target-services)
                                        (map live-service-canonical-name
                                             live-services)))
             (service-files (map shepherd-service-file target-services)))
        (eval #~(primitive-load #$(upgrade-services-program service-files
                                                            to-start
                                                            to-unload
                                                            to-restart)))))))


;;;
;;; Bootloader configuration.
;;;

(define (install-bootloader-program installer bootloader-package bootcfg
                                    bootcfg-file device target)
  "Return an executable store item that, upon being evaluated, will install
BOOTCFG to BOOTCFG-FILE, a target file name, on DEVICE, a file system device,
at TARGET, a mount point, and subsequently run INSTALLER from
BOOTLOADER-PACKAGE."
  (program-file
   "install-bootloader.scm"
   (with-extensions (list guile-gcrypt)
     (with-imported-modules `(,@(source-module-closure
                                 '((gnu build bootloader)
                                   (gnu build install)
                                   (guix store)
                                   (guix utils))
                                 #:select? not-config?)
                              ((guix config) => ,(make-config.scm)))
       #~(begin
           (use-modules (gnu build bootloader)
                        (gnu build install)
                        (guix build utils)
                        (guix store)
                        (guix utils)
                        (ice-9 binary-ports)
                        (srfi srfi-34)
                        (srfi srfi-35))

           (let* ((gc-root (string-append #$target %gc-roots-directory "/bootcfg"))
                  (new-gc-root (string-append gc-root ".new")))
             ;; #$bootcfg has dependencies.
             ;; The bootloader magically loads the configuration from
             ;; (string-append #$target #$bootcfg-file) (for example
             ;; "/boot/grub/grub.cfg").
             ;; If we didn't do something special, the garbage collector
             ;; would remove the dependencies of #$bootcfg.
             ;; Register #$bootcfg as a GC root.
             ;; Preserve the previous activation's garbage collector root
             ;; until the bootloader installer has run, so that a failure in
             ;; the bootloader's installer script doesn't leave the user with
             ;; a broken installation.
             (switch-symlinks new-gc-root #$bootcfg)
             (install-boot-config #$bootcfg #$bootcfg-file #$target)
             (when #$installer
               (catch #t
                 (lambda ()
                   (#$installer #$bootloader-package #$device #$target))
                 (lambda args
                   (delete-file new-gc-root)
                   (apply throw args))))
             ;; We are sure that the installation of the bootloader
             ;; succeeded, so we can replace the old GC root by the new
             ;; GC root now.
             (rename-file new-gc-root gc-root)))))))

(define* (install-bootloader eval configuration bootcfg
                             #:key
                             (run-installer? #t)
                             (target "/"))
  "Using EVAL, a monadic procedure taking a single G-Expression as an argument,
configure the bootloader on TARGET such that OS will be booted by default and
additional configurations specified by MENU-ENTRIES can be selected."
  (let* ((bootloader (bootloader-configuration-bootloader configuration))
         (installer (and run-installer?
                         (bootloader-installer bootloader)))
         (package (bootloader-package bootloader))
         (device (bootloader-configuration-target configuration))
         (bootcfg-file (bootloader-configuration-file bootloader)))
    (eval #~(primitive-load #$(install-bootloader-program installer
                                                          package
                                                          bootcfg
                                                          bootcfg-file
                                                          device
                                                          target)))))
