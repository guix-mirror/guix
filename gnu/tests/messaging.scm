;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu tests messaging)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services messaging)
  #:use-module (gnu services networking)
  #:use-module (gnu packages messaging)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-prosody))

(define (run-xmpp-test name xmpp-service pid-file create-account)
  "Run a test of an OS running XMPP-SERVICE, which writes its PID to PID-FILE."
  (define os
    (marionette-operating-system
     (simple-operating-system (dhcp-client-service)
                              xmpp-service)
     #:imported-modules '((gnu services herd))))

  (define port 15222)

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((,port . 5222)))))

  (define username "alice")
  (define server "localhost")
  (define jid (string-append username "@" server))
  (define password "correct horse battery staple")
  (define message "hello world")
  (define witness "/tmp/freetalk-witness")

  (define script.ft
    (scheme-file
     "script.ft"
     #~(begin
         (define (handle-received-message time from nickname message)
           (define (touch file-name)
             (call-with-output-file file-name (const #t)))
           (when (equal? message #$message)
             (touch #$witness)))
         (add-hook! ft-message-receive-hook handle-received-message)

         (ft-set-jid! #$jid)
         (ft-set-password! #$password)
         (ft-set-server! #$server)
         (ft-set-port! #$port)
         (ft-set-sslconn! #f)
         (ft-connect-blocking)
         (ft-send-message #$jid #$message)

         (ft-set-daemon)
         (ft-main-loop))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$vm)))

          (define (host-wait-for-file file)
            ;; Wait until FILE exists in the host.
            (let loop ((i 60))
              (cond ((file-exists? file)
                     #t)
                    ((> i 0)
                     (begin
                       (sleep 1))
                     (loop (- i 1)))
                    (else
                     (error "file didn't show up" file)))))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "xmpp")

          ;; Wait for XMPP service to be up and running.
          (test-eq "service running"
            'running!
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'xmpp-daemon)
                'running!)
             marionette))

          ;; Check XMPP service's PID.
          (test-assert "service process id"
            (let ((pid (number->string (wait-for-file #$pid-file
                                                      marionette))))
              (marionette-eval `(file-exists? (string-append "/proc/" ,pid))
                               marionette)))

          ;; Alice sends an XMPP message to herself, with Freetalk.
          (test-assert "client-to-server communication"
            (let ((freetalk-bin (string-append #$freetalk "/bin/freetalk")))
              (marionette-eval '(system* #$create-account #$jid #$password)
                               marionette)
              ;; Freetalk requires write access to $HOME.
              (setenv "HOME" "/tmp")
              (system* freetalk-bin "-s" #$script.ft)
              (host-wait-for-file #$witness)))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation name test))

(define %create-prosody-account
  (program-file
   "create-account"
   #~(begin
       (use-modules (ice-9 match))
       (match (command-line)
         ((command jid password)
          (let ((password-input (format #f "\"~a~%~a\"" password password))
                (prosodyctl #$(file-append prosody "/bin/prosodyctl")))
            (system (string-join
                     `("echo" ,password-input "|" ,prosodyctl "adduser" ,jid)
                     " "))))))))

(define %test-prosody
  (let* ((config (prosody-configuration
                  (disable-sasl-mechanisms '())
                  (virtualhosts
                   (list
                    (virtualhost-configuration
                     (domain "localhost")))))))
    (system-test
     (name "prosody")
     (description "Connect to a running Prosody daemon.")
     (value (run-xmpp-test name
                           (service prosody-service-type config)
                           (prosody-configuration-pidfile config)
                           %create-prosody-account)))))
