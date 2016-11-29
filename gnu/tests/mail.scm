;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
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

(define-module (gnu tests mail)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system grub)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services mail)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:export (%test-opensmtpd))

(define %opensmtpd-os
  (operating-system
    (host-name "komputilo")
    (timezone "Europe/Berlin")
    (locale "en_US.UTF-8")
    (bootloader (grub-configuration (device #f)))
    (file-systems %base-file-systems)
    (firmware '())
    (services (cons*
               (dhcp-client-service)
               (service opensmtpd-service-type
                        (opensmtpd-configuration
                         (config-file
                          (plain-file "smtpd.conf" "
listen on 0.0.0.0
accept from any for local deliver to mbox
"))))
               %base-services))))

(define (run-opensmtpd-test)
  "Return a test of an OS running OpenSMTPD service."
  (mlet* %store-monad ((command (system-qemu-image/shared-store-script
                                 (marionette-operating-system
                                  %opensmtpd-os
                                  #:imported-modules '((gnu services herd)))
                                 #:graphic? #f)))
    (define test
      (with-imported-modules '((gnu build marionette))
        #~(begin
            (use-modules (rnrs base)
                         (srfi srfi-64)
                         (ice-9 rdelim)
                         (ice-9 regex)
                         (gnu build marionette))

            (define marionette
              (make-marionette
               ;; Enable TCP forwarding of the guest's port 25.
               '(#$command "-net" "user,hostfwd=tcp::1025-:25")))

            (define (read-reply-code port)
              "Read a SMTP reply from PORT and return its reply code."
              (let* ((line      (read-line port))
                     (mo        (string-match "([0-9]+)([ -]).*" line))
                     (code      (string->number (match:substring mo 1)))
                     (finished? (string= " " (match:substring mo 2))))
                (if finished?
                    code
                    (read-reply-code port))))

            (mkdir #$output)
            (chdir #$output)

            (test-begin "opensmptd")

            (test-assert "service is running"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'smtpd)
                  #t)
               marionette))

            (test-assert "mbox is empty"
              (marionette-eval
               '(and (file-exists? "/var/mail")
                     (not (file-exists? "/var/mail/root")))
               marionette))

            (test-eq "accept an email"
              #t
              (let* ((smtp (socket AF_INET SOCK_STREAM 0))
                     (addr (make-socket-address AF_INET INADDR_LOOPBACK 1025)))
                (connect smtp addr)
                ;; Be greeted.
                (read-reply-code smtp)             ;220
                ;; Greet the server.
                (write-line "EHLO somehost" smtp)
                (read-reply-code smtp)             ;250
                ;; Set sender email.
                (write-line "MAIL FROM: <someone>" smtp)
                (read-reply-code smtp)             ;250
                ;; Set recipient email.
                (write-line "RCPT TO: <root>" smtp)
                (read-reply-code smtp)             ;250
                ;; Send message.
                (write-line "DATA" smtp)
                (read-reply-code smtp)             ;354
                (write-line "Subject: Hello" smtp)
                (newline smtp)
                (write-line "Nice to meet you!" smtp)
                (write-line "." smtp)
                (read-reply-code smtp)             ;250
                ;; Say goodbye.
                (write-line "QUIT" smtp)
                (read-reply-code smtp)             ;221
                (close smtp)
                #t))

            (test-assert "mail arrived"
              (marionette-eval
               '(begin
                  (use-modules (ice-9 popen)
                               (ice-9 rdelim))

                  (define (queue-empty?)
                    (eof-object?
                     (read-line
                      (open-input-pipe "smtpctl show queue"))))

                  (let wait ()
                    (if (queue-empty?)
                        (file-exists? "/var/mail/root")
                        (begin (sleep 1) (wait)))))
               marionette))

            (test-end)
            (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

    (gexp->derivation "opensmtpd-test" test)))

(define %test-opensmtpd
  (system-test
   (name "opensmtpd")
   (description "Send an email to a running OpenSMTPD server.")
   (value (run-opensmtpd-test))))
