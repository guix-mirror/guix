;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu tests rsync)
  #:use-module (gnu packages rsync)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services rsync)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-rsync))

(define* (run-rsync-test rsync-os #:optional (rsync-port 873))
  "Run tests in %RSYNC-OS, which has rsync running and listening on
PORT."
  (define os
    (marionette-operating-system
     rsync-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "rsync")

          ;; Wait for rsync to be up and running.
          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))

                ;; Make sure the 'rsync' command is found.
                (setenv "PATH" "/run/current-system/profile/bin")

                (start-service 'rsync))
             marionette))

          ;; Make sure the PID file is created.
          (test-assert "PID file"
            (marionette-eval
             '(file-exists? "/var/run/rsyncd/rsyncd.pid")
             marionette))

          (test-assert "Test file copied to share"
            (marionette-eval
             '(begin
                (call-with-output-file "/tmp/input"
                  (lambda (port)
                    (display "test-file-contents\n" port)))
                (zero?
                 (system* "rsync" "/tmp/input"
                          (string-append "rsync://localhost:"
                                         (number->string #$rsync-port)
                                         "/files/input"))))
             marionette))

          (test-equal "Test file correctly received from share"
            "test-file-contents"
            (marionette-eval
             '(begin
                (use-modules (ice-9 rdelim))
                (zero?
                 (system* "rsync"
                          (string-append "rsync://localhost:"
                                         (number->string #$rsync-port)
                                         "/files/input")
                          "/tmp/output"))
                (call-with-input-file "/tmp/output"
                  (lambda (port)
                    (read-line port))))
             marionette))

          (test-equal "Test file not copied to read-only share"
            1                                  ;see "EXIT VALUES" in rsync(1)
            (marionette-eval
             '(status:exit-val
               (system* "rsync" "/tmp/input"
                        (string-append "rsync://localhost:"
                                       (number->string #$rsync-port)
                                       "/read-only/input")))
             marionette))

          (test-equal "Test file correctly received from read-only share"
            "\"Hi!\" from the read-only share."
            (marionette-eval
             '(begin
                (use-modules (ice-9 rdelim))

                (call-with-output-file "/srv/read-only/the-file"
                  (lambda (port)
                    (display "\"Hi!\" from the read-only share." port)))

                (zero?
                 (system* "rsync"
                          (string-append "rsync://localhost:"
                                         (number->string #$rsync-port)
                                         "/read-only/the-file")
                          "/tmp/output"))
                (call-with-input-file "/tmp/output" read-line))
             marionette))

          (test-end))))

  (gexp->derivation "rsync-test" test))

(define* %rsync-os
  ;; Return operating system under test.
  (let ((base-os
         (simple-operating-system
          (service dhcp-client-service-type)
          (service rsync-service-type
                   (rsync-configuration
                    (modules (list (rsync-module
                                    (name "read-only")
                                    (file-name "/srv/read-only"))
                                   (rsync-module
                                    (name "files")
                                    (file-name "/srv/read-write")
                                    (read-only? #f)))))))))
    (operating-system
      (inherit base-os)
      (packages (cons* rsync
                       (operating-system-packages base-os))))))

(define %test-rsync
  (system-test
   (name "rsync")
   (description "Connect to a running RSYNC server.")
   (value (run-rsync-test %rsync-os))))
