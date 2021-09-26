;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu tests desktop)
  #:use-module (gnu tests)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu system vm)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (%test-elogind))


;;;
;;; Elogind.
;;;

(define (run-elogind-test vm)
  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build syscalls))
      #~(begin
          (use-modules (gnu build marionette)
                       (guix build syscalls)
                       (srfi srfi-64))

          (define marionette
            (make-marionette '(#$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "elogind")

          ;; Log in as root on tty1, and check what 'loginctl' returns.
          (test-equal "login on tty1"
            '(("c1" "0" "root" "seat0" "tty1")      ;session
              ("seat0")                             ;seat
              ("0" "root"))                         ;user

            (begin
              ;; Wait for tty1.
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'term-tty1))
               marionette)
              (marionette-control "sendkey ctrl-alt-f1" marionette)

              ;; Now we can type.
              (marionette-type "root\n" marionette)
              (marionette-type "loginctl list-users --no-legend > users\n"
                               marionette)
              (marionette-type "loginctl list-seats --no-legend > seats\n"
                               marionette)
              (marionette-type "loginctl list-sessions --no-legend > sessions\n"
                               marionette)


              ;; Read the three files.
              (marionette-eval '(use-modules (rnrs io ports)) marionette)
              (let ((guest-file (lambda (file)
                                  (string-tokenize
                                   (wait-for-file file marionette
                                                  #:read 'get-string-all)))))
                (list (guest-file "/root/sessions")
                      (guest-file "/root/seats")
                      (guest-file "/root/users")))))

          (test-end))))

  (gexp->derivation "elogind" test))

(define %test-elogind
  (system-test
   (name "elogind")
   (description
    "Test whether we can log in when elogind is enabled, and whether
'loginctl' reports accurate user, session, and seat information.")
   (value
    (let ((os (marionette-operating-system
               (simple-operating-system
                (service elogind-service-type)
                (service polkit-service-type)
                (service dbus-root-service-type))
               #:imported-modules '((gnu services herd)
                                    (guix combinators)))))
      (run-elogind-test (virtual-machine os))))))
