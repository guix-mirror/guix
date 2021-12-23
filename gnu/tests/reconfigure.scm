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

(define-module (gnu tests reconfigure)
  #:use-module (gnu bootloader)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix scripts system reconfigure)
  #:use-module (guix store)
  #:export (%test-switch-to-system
            %test-upgrade-services
            %test-install-bootloader))

;;; Commentary:
;;;
;;; Test in-place system reconfiguration: advancing the system generation on a
;;; running instance of the Guix System.
;;;
;;; Code:

(define* (run-switch-to-system-test)
  "Run a test of an OS running SWITCH-SYSTEM-PROGRAM, which creates a new
generation of the system profile."
  (define os
    (marionette-operating-system
     (operating-system
       (inherit (simple-operating-system))
       (users (cons (user-account
                     (name "jakob")
                     (group "users")
                     (home-directory "/home/jakob"))
                    %base-user-accounts)))
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm (virtual-machine os))

  (define (test script)
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$vm)))

          ;; Return the names of the generation symlinks on MARIONETTE.
          (define (system-generations marionette)
            (marionette-eval
             '(begin
                (use-modules (ice-9 ftw)
                             (srfi srfi-1))
                (let* ((profile-dir "/var/guix/profiles/")
                       (entries (map first (cddr (file-system-tree profile-dir)))))
                  (remove (lambda (entry)
                            (member entry '("per-user" "system")))
                          entries)))
             marionette))

          (test-runner-current (system-test-runner #$output))
          (test-begin "switch-to-system")

          (let ((generations-prior (system-generations marionette)))
            (test-assert "script successfully evaluated"
              (marionette-eval
               '(primitive-load #$script)
               marionette))

            (test-equal "script created new generation"
              (length (system-generations marionette))
              (1+ (length generations-prior)))

            (test-assert "script activated the new generation"
              (and (eqv? 'symlink
                         (marionette-eval
                          '(stat:type (lstat "/run/current-system"))
                          marionette))
                   (string= #$os
                            (marionette-eval
                             '(readlink "/run/current-system")
                             marionette))))

            (test-assert "script activated user accounts"
              (marionette-eval
               '(string-contains (call-with-input-file "/etc/passwd"
                                   (lambda (port)
                                     (get-string-all port)))
                                 "jakob")
               marionette)))

          (test-end))))

  (gexp->derivation "switch-to-system" (test (switch-system-program os))))

(define* (run-upgrade-services-test)
  "Run a test of an OS running UPGRADE-SERVICES-PROGRAM, which upgrades the
Shepherd (PID 1) by unloading obsolete services and loading new services."
  (define os
    (marionette-operating-system
     (simple-operating-system)
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm (virtual-machine os))

  (define dummy-service
    ;; Shepherd service that does nothing, for the sole purpose of ensuring
    ;; that it is properly installed and started by the script.
    (shepherd-service (provision '(dummy))
                      (start #~(const #t))
                      (stop #~(const #t))
                      (respawn? #f)))

  (define (test enable-dummy disable-dummy)
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$vm)))

          ;; Return the names of the running services on MARIONETTE.
          (define (running-services marionette)
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (map live-service-canonical-name (current-services)))
             marionette))

          (test-runner-current (system-test-runner #$output))
          (test-begin "upgrade-services")

          (let ((services-prior (running-services marionette)))
            (test-assert "script successfully evaluated"
              (marionette-eval
               '(primitive-load #$enable-dummy)
               marionette))

            (test-assert "script started new service"
              (and (not (memq 'dummy services-prior))
                   (memq 'dummy (running-services marionette))))

            (test-assert "script successfully evaluated"
              (marionette-eval
               '(primitive-load #$disable-dummy)
               marionette))

            (test-assert "script stopped obsolete service"
              (not (memq 'dummy (running-services marionette)))))

          (test-end))))

  (gexp->derivation
   "upgrade-services"
   (let* ((file (shepherd-service-file dummy-service))
          (enable (upgrade-services-program (list file) '(dummy) '() '()))
          (disable (upgrade-services-program '() '() '(dummy) '())))
     (test enable disable))))

(define* (run-install-bootloader-test)
  "Run a test of an OS running INSTALL-BOOTLOADER-PROGRAM, which installs a
bootloader's configuration file."
  (define os
    (marionette-operating-system
     (simple-operating-system)
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm (virtual-machine
              (operating-system os)
              (volatile? #f)))

  (define (test script)
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (ice-9 regex)
                       (srfi srfi-1)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$vm)))

          ;; Return the system generation paths that have GRUB menu entries.
          (define (generations-in-grub-cfg marionette)
            (let ((grub-cfg (marionette-eval
                             '(begin
                                (call-with-input-file "/boot/grub/grub.cfg"
                                  (lambda (port)
                                    (get-string-all port))))
                             marionette)))
              (map (lambda (parameter)
                     (second (string-split (match:substring parameter) #\=)))
                   (list-matches "system=[^ ]*" grub-cfg))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "install-bootloader")

          (test-assert "no prior menu entry for system generation"
            (not (member #$os (generations-in-grub-cfg marionette))))

          (test-assert "script successfully evaluated"
            (marionette-eval
             '(primitive-load #$script)
             marionette))

          (test-assert "menu entry created for system generation"
            (member #$os (generations-in-grub-cfg marionette)))

          (test-end))))

  (let* ((bootloader ((compose bootloader-configuration-bootloader
                               operating-system-bootloader)
                      os))
         ;; The typical use-case for 'install-bootloader-program' is to read
         ;; the boot parameters for the existing menu entries on the system,
         ;; parse them with 'boot-parameters->menu-entry', and pass the
         ;; results to 'operating-system-bootcfg'. However, to obtain boot
         ;; parameters, we would need to start the marionette, which we should
         ;; ideally avoid doing outside of the 'test' G-Expression. Thus, we
         ;; generate a bootloader configuration for the script as if there
         ;; were no existing menu entries. In the grand scheme of things, this
         ;; matters little -- these tests should not make assertions about the
         ;; behavior of 'operating-system-bootcfg'.
         (bootcfg (operating-system-bootcfg os '()))
         (bootcfg-file (bootloader-configuration-file bootloader)))
    (gexp->derivation
     "install-bootloader"
     ;; Due to the read-only nature of the virtual machines used in the system
     ;; test suite, the bootloader installer script is omitted. 'grub-install'
     ;; would attempt to write directly to the virtual disk if the
     ;; installation script were run.
     (test
      (install-bootloader-program #f #f #f bootcfg bootcfg-file '(#f) "/")))))


(define %test-switch-to-system
  (system-test
   (name "switch-to-system")
   (description "Create a new generation of the system profile.")
   (value (run-switch-to-system-test))))

(define %test-upgrade-services
  (system-test
   (name "upgrade-services")
   (description "Upgrade the Shepherd by unloading obsolete services and
loading new services.")
   (value (run-upgrade-services-test))))

(define %test-install-bootloader
  (system-test
   (name "install-bootloader")
   (description "Install a bootloader and its configuration file.")
   (value (run-install-bootloader-test))))
