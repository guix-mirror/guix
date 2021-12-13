;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu tests package-management)
  #:use-module (gnu packages base)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gnu services nix)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (%test-nix))

;;; Commentary:
;;;
;;; This module provides a test definition for the nix-daemon
;;;
;;; Code:

(define* (run-nix-test name test-os)
  "Run tests in TEST-OS, which has nix-daemon running."
  (define os
    (marionette-operating-system
     test-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((8080 . 80)))
     (memory-size 1024)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11)
                       (srfi srfi-64)
                       (gnu build marionette)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin #$name)

          ;; XXX: Shepherd reads the config file *before* binding its control
          ;; socket, so /var/run/shepherd/socket might not exist yet when the
          ;; 'marionette' service is started.
          (test-assert "shepherd socket ready"
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (let loop ((i 10))
                  (cond ((file-exists? (%shepherd-socket-file))
                         #t)
                        ((> i 0)
                         (sleep 1)
                         (loop (- i 1)))
                        (else
                         'failure))))
             marionette))

          (test-assert "Nix daemon running"
            (marionette-eval
             '(begin
                ;; Wait for nix-daemon to be up and running.
                (start-service 'nix-daemon)
                (with-output-to-file "guix-test.nix"
                  (lambda ()
                    (display "\
with import <nix/config.nix>;

derivation {
  system = builtins.currentSystem;
  name = \"guix-test\";
  builder = shell;
  args = [\"-c\" \"mkdir $out\\necho FOO > $out/foo\"];
  PATH = coreutils;
}
")))
                (zero? (system* (string-append #$nix "/bin/nix-build")
                                "--substituters" "" "--debug" "--no-out-link"
                                "guix-test.nix")))
             marionette))

	  (test-end))))

  (gexp->derivation (string-append name "-test") test))

(define %nix-os
  ;; Return operating system under test.
  (let ((base-os
         (simple-operating-system
          (service nix-service-type)
	  (service dhcp-client-service-type))))
    (operating-system
      (inherit base-os)
      (packages (cons nix (operating-system-packages base-os))))))

(define %test-nix
  (system-test
   (name "nix")
   (description "Connect to a running nix-daemon")
   (value (run-nix-test name %nix-os))))

;;; package-management.scm ends here
