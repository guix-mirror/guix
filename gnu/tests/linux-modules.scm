;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 raid5atemyhomework <raid5atemyhomework@protonmail.com>
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
  #:use-module (gnu services)
  #:use-module (gnu services linux)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:export (%test-loadable-kernel-modules-0
            %test-loadable-kernel-modules-1
            %test-loadable-kernel-modules-2
            %test-loadable-kernel-modules-service-0
            %test-loadable-kernel-modules-service-1
            %test-loadable-kernel-modules-service-2))

;;; Commentary:
;;;
;;; Test <operating-system> kernel-loadable-modules.
;;;
;;; Code:

(define* (modules-loaded?-program os modules)
  "Return an executable store item that, upon being evaluated, will verify
that MODULES are actually loaded."
  (program-file
   "verify-kernel-modules-loaded.scm"
   #~(begin
     (use-modules (ice-9 rdelim)
                  (ice-9 popen)
                  (srfi srfi-1)
                  (srfi srfi-13))
     (let* ((port (open-input-pipe (string-append #$kmod "/bin/lsmod")))
            (lines (string-split (read-string port) #\newline))
            (separators (char-set #\space #\tab))
            (modules (map (lambda (line)
                            (string-take line
                                         (or (string-index line separators)
                                             0)))
                          lines))
            (status (close-pipe port)))
       (and (= status 0)
            (and-map (lambda (module)
                       (member module modules string=?))
                     '#$modules))))))

(define* (run-loadable-kernel-modules-test-base base-os module-names)
  "Run a test of BASE-OS, verifying that MODULE-NAMES are loaded in memory."
  (define os
    (marionette-operating-system
     base-os
     #:imported-modules '((guix combinators))))

  (define vm (virtual-machine os))

  (define (test script)
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "loadable-kernel-modules")
          (test-assert "script successfully evaluated"
            (marionette-eval
             '(primitive-load #$script)
             marionette))
          (test-end))))

  (gexp->derivation "loadable-kernel-modules"
                    (test (modules-loaded?-program os module-names))))

(define* (run-loadable-kernel-modules-test module-packages module-names)
  "Run a test of an OS having MODULE-PACKAGES, and verify that MODULE-NAMES
are loaded in memory."
  (run-loadable-kernel-modules-test-base
    (operating-system
      (inherit (simple-operating-system))
      (services (cons (service kernel-module-loader-service-type module-names)
                      (operating-system-user-services
                       (simple-operating-system))))
      (kernel-loadable-modules module-packages))
    module-names))

(define* (run-loadable-kernel-modules-service-test module-packages module-names)
  "Run a test of an OS having MODULE-PACKAGES, which are loaded by creating a
service that extends LINUXL-LOADABLE-MODULE-SERVICE-TYPE. Then verify that
MODULE-NAMES are loaded in memory."
  (run-loadable-kernel-modules-test-base
    (operating-system
      (inherit (simple-operating-system))
      (services (cons* (simple-service 'installing-module
                                       linux-loadable-module-service-type
                                       module-packages)
                       (service kernel-module-loader-service-type module-names)
                       (operating-system-user-services
                        (simple-operating-system)))))
    module-names))

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
           (list acpi-call-linux-module
                 (package
                   (inherit ddcci-driver-linux)
                   (arguments
                    `(#:linux #f
                      ,@(strip-keyword-arguments '(#:linux)
                                                 (package-arguments
                                                  ddcci-driver-linux))))))
           '("acpi_call" "ddcci")))))

(define %test-loadable-kernel-modules-service-0
  (system-test
   (name "loadable-kernel-modules-service-0")
   (description "Tests loadable kernel modules extensible service with no
extra modules.")
   (value (run-loadable-kernel-modules-service-test '() '()))))

(define %test-loadable-kernel-modules-service-1
  (system-test
   (name "loadable-kernel-modules-service-1")
   (description "Tests loadable kernel modules extensible service with one
extra module.")
   (value (run-loadable-kernel-modules-service-test
           (list ddcci-driver-linux)
           '("ddcci")))))

(define %test-loadable-kernel-modules-service-2
  (system-test
   (name "loadable-kernel-modules-service-2")
   (description "Tests loadable kernel modules extensible service with two
extra modules.")
   (value (run-loadable-kernel-modules-service-test
           (list acpi-call-linux-module
                 (package
                   (inherit ddcci-driver-linux)
                   (arguments
                    `(#:linux #f
                      ,@(strip-keyword-arguments '(#:linux)
                                                 (package-arguments
                                                  ddcci-driver-linux))))))
           '("acpi_call" "ddcci")))))
