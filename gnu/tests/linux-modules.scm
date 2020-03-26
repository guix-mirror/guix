;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu tests linux-modules)
  #:use-module (gnu packages linux)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:export (%test-loadable-kernel-modules-0
            %test-loadable-kernel-modules-1
            %test-loadable-kernel-modules-2))

;;; Commentary:
;;;
;;; Test <operating-system> kernel-loadable-modules.
;;;
;;; Code:

(define* (module-loader-program os modules)
  "Return an executable store item that, upon being evaluated, will dry-run
load MODULES."
  (program-file
   "load-kernel-modules.scm"
   (with-imported-modules (source-module-closure '((guix build utils)))
     #~(begin
         (use-modules (guix build utils))
         (for-each (lambda (module)
                     (invoke (string-append #$kmod "/bin/modprobe") "-n" "--"
                             module))
                   '#$modules)))))

(define* (run-loadable-kernel-modules-test module-packages module-names)
  "Run a test of an OS having MODULE-PACKAGES, and modprobe MODULE-NAMES."
  (define os
    (marionette-operating-system
     (operating-system
      (inherit (simple-operating-system))
      (kernel-loadable-modules module-packages))
     #:imported-modules '((guix combinators))))
  (define vm (virtual-machine os))
  (define (test script)
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))
          (define marionette
            (make-marionette (list #$vm)))
          (mkdir #$output)
          (chdir #$output)
          (test-begin "loadable-kernel-modules")
          (test-assert "script successfully evaluated"
            (marionette-eval
             '(primitive-load #$script)
             marionette))
          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))
  (gexp->derivation "loadable-kernel-modules" (test (module-loader-program os module-names))))

(define %test-loadable-kernel-modules-0
  (system-test
   (name "loadable-kernel-modules-0")
   (description "Tests loadable kernel modules facility of <operating-system>
with no extra modules.")
   (value (run-loadable-kernel-modules-test '() '()))))

(define %test-loadable-kernel-modules-1
  (system-test
   (name "loadable-kernel-modules-1")
   (description "Tests loadable kernel modules facility of <operating-system>
with one extra module.")
   (value (run-loadable-kernel-modules-test
           (list ddcci-driver-linux)
           '("ddcci")))))

(define %test-loadable-kernel-modules-2
  (system-test
   (name "loadable-kernel-modules-2")
   (description "Tests loadable kernel modules facility of <operating-system>
with two extra modules.")
   (value (run-loadable-kernel-modules-test
           (list acpi-call-linux-module ddcci-driver-linux)
           '("acpi_call" "ddcci")))))
