;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu tests security-token)
  #:use-module (gnu tests)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services security-token)
  #:use-module (guix gexp)
  #:export (%test-pcscd))

(define %pcscd-os
  (simple-operating-system
   (service pcscd-service-type)))

(define* (run-pcscd-test)
  "Run tests of 'pcscd-service-type'."
  (define os
    (marionette-operating-system
     %pcscd-os
     #:imported-modules '((gnu services herd))
     #:requirements '(pcscd)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))
          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "pcscd")

          (test-assert "pcscd is alive"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (live-service-running
                 (find (lambda (live)
                         (memq 'pcscd (live-service-provision live)))
                       (current-services))))
             marionette))

          (test-end))))

  (gexp->derivation "pcscd" test))

(define %test-pcscd
  (system-test
   (name "pcscd")
   (description "Test a running pcscd daemon.")
   (value (run-pcscd-test))))
