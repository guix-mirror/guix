;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
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
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services mail)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (ice-9 ftw)
  #:export (%test-opensmtpd
            %test-exim))

(define %opensmtpd-os
  (simple-operating-system
   (dhcp-client-service)
   (service opensmtpd-service-type
            (opensmtpd-configuration
             (config-file
              (plain-file "smtpd.conf" "
listen on 0.0.0.0
accept from any for local deliver to mbox
"))))))

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


(define %exim-os
  (simple-operating-system
   (dhcp-client-service)
   (service mail-aliases-service-type '())
   (service exim-service-type
            (exim-configuration
             (config-file
              (plain-file "exim.conf" "
primary_hostname = komputilo
domainlist local_domains = @
domainlist relay_to_domains =
hostlist   relay_from_hosts = localhost

never_users =

acl_smtp_rcpt = acl_check_rcpt
acl_smtp_data = acl_check_data

begin acl

acl_check_rcpt:
  accept
acl_check_data:
  accept
"))))))

(define (run-exim-test)
  "Return a test of an OS running an Exim service."
  (mlet* %store-monad ((command (system-qemu-image/shared-store-script
                                 (marionette-operating-system
                                  %exim-os
                                  #:imported-modules '((gnu services herd)))
                                 #:graphic? #f)))
    (define test
      (with-imported-modules '((gnu build marionette)
                               (ice-9 ftw))
        #~(begin
            (use-modules (rnrs base)
                         (srfi srfi-64)
                         (ice-9 ftw)
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

            (define smtp (socket AF_INET SOCK_STREAM 0))
            (define addr (make-socket-address AF_INET INADDR_LOOPBACK 1025))

            (mkdir #$output)
            (chdir #$output)

            (test-begin "exim")

            (test-assert "service is running"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'exim)
                  #t)
               marionette))

            (sleep 1) ;; give the service time to start talking

            (connect smtp addr)
            ;; Be greeted.
            (test-eq "greeting received"
              220 (read-reply-code smtp))
            ;; Greet the server.
            (write-line "EHLO somehost" smtp)
            (test-eq "greeting successful"
              250 (read-reply-code smtp))
            ;; Set sender email.
            (write-line "MAIL FROM: test@example.com" smtp)
            (test-eq "sender set"
              250 (read-reply-code smtp)) ;250
            ;; Set recipient email.
            (write-line "RCPT TO: root@komputilo" smtp)
            (test-eq "recipient set"
              250 (read-reply-code smtp)) ;250
            ;; Send message.
            (write-line "DATA" smtp)
            (test-eq "data begun"
              354 (read-reply-code smtp)) ;354
            (write-line "Subject: Hello" smtp)
            (newline smtp)
            (write-line "Nice to meet you!" smtp)
            (write-line "." smtp)
            (test-eq "message sent"
              250 (read-reply-code smtp)) ;250
            ;; Say goodbye.
            (write-line "QUIT" smtp)
            (test-eq "quit successful"
              221 (read-reply-code smtp)) ;221
            (close smtp)

            (test-eq "the email is received"
              1
              (marionette-eval
               '(begin
                  (use-modules (ice-9 ftw))
                  (length (scandir "/var/spool/exim/msglog"
                                   (lambda (x) (not (string-prefix? "." x))))))
               marionette))

            (test-end)
            (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

    (gexp->derivation "exim-test" test)))

(define %test-exim
  (system-test
   (name "exim")
   (description "Send an email to a running an Exim server.")
   (value (run-exim-test))))
