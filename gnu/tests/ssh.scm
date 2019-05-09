;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu tests ssh)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu packages ssh)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-openssh
            %test-dropbear))

(define* (run-ssh-test name ssh-service pid-file
                       #:key (sftp? #f) (test-getlogin? #t))
  "Run a test of an OS running SSH-SERVICE, which writes its PID to PID-FILE.
SSH-SERVICE must be configured to listen on port 22 and to allow for root and
empty-password logins.

When SFTP? is true, run an SFTP server test."
  (define os
    (marionette-operating-system
     (simple-operating-system (service dhcp-client-service-type) ssh-service)
     #:imported-modules '((gnu services herd)
                          (guix combinators))))
  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((2222 . 22)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      (with-extensions (list guile-ssh)
        #~(begin
            (use-modules (gnu build marionette)
                         (srfi srfi-26)
                         (srfi srfi-64)
                         (ice-9 textual-ports)
                         (ice-9 match)
                         (ssh session)
                         (ssh auth)
                         (ssh channel)
                         (ssh popen)
                         (ssh sftp))

            (define marionette
              ;; Enable TCP forwarding of the guest's port 22.
              (make-marionette (list #$vm)))

            (define (make-session-for-test)
              "Make a session with predefined parameters for a test."
              (make-session #:user "root"
                            #:port 2222
                            #:host "localhost"
                            #:log-verbosity 'protocol))

            (define (call-with-connected-session proc)
              "Call the one-argument procedure PROC with a freshly created and
connected SSH session object, return the result of the procedure call.  The
session is disconnected when the PROC is finished."
              (let ((session (make-session-for-test)))
                (dynamic-wind
                  (lambda ()
                    (let ((result (connect! session)))
                      (unless (equal? result 'ok)
                        (error "Could not connect to a server"
                               session result))))
                  (lambda () (proc session))
                  (lambda () (disconnect! session)))))

            (define (call-with-connected-session/auth proc)
              "Make an authenticated session.  We should be able to connect as
root with an empty password."
              (call-with-connected-session
               (lambda (session)
                 ;; Try the simple authentication methods.  Dropbear requires
                 ;; 'none' when there are no passwords, whereas OpenSSH accepts
                 ;; 'password' with an empty password.
                 (let loop ((methods (list (cut userauth-password! <> "")
                                           (cut userauth-none! <>))))
                   (match methods
                     (()
                      (error "all the authentication methods failed"))
                     ((auth rest ...)
                      (match (pk 'auth (auth session))
                        ('success
                         (proc session))
                        ('denied
                         (loop rest)))))))))

            (mkdir #$output)
            (chdir #$output)

            (test-begin "ssh-daemon")

            ;; Wait for sshd to be up and running.
            (test-assert "service running"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'ssh-daemon))
               marionette))

            ;; Check sshd's PID file.
            (test-equal "sshd PID"
              (wait-for-file #$pid-file marionette)
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd)
                               (srfi srfi-1))

                  (live-service-running
                   (find (lambda (live)
                           (memq 'ssh-daemon
                                 (live-service-provision live)))
                         (current-services))))
               marionette))

            ;; Connect to the guest over SSH.  Make sure we can run a shell
            ;; command there.
            (test-equal "shell command"
              'hello
              (call-with-connected-session/auth
               (lambda (session)
                 ;; FIXME: 'get-server-public-key' segfaults.
                 ;; (get-server-public-key session)
                 (let ((channel (make-channel session)))
                   (channel-open-session channel)
                   (channel-request-exec channel "echo hello > /root/witness")
                   (and (zero? (channel-get-exit-status channel))
                        (wait-for-file "/root/witness" marionette))))))

            ;; Check whether the 'getlogin' procedure returns the right thing.
            (unless #$test-getlogin?
              (test-skip 1))
            (test-equal "getlogin"
              '(0 "root")
              (call-with-connected-session/auth
               (lambda (session)
                 (let* ((pipe   (open-remote-input-pipe
                                 session
                                 "guile -c '(display (getlogin))'"))
                        (output (get-string-all pipe))
                        (status (channel-get-exit-status pipe)))
                   (list status output)))))

            ;; Connect to the guest over SFTP.  Make sure we can write and
            ;; read a file there.
            (unless #$sftp?
              (test-skip 1))
            (test-equal "SFTP file writing and reading"
              'hello
              (call-with-connected-session/auth
               (lambda (session)
                 (let ((sftp-session (make-sftp-session session))
                       (witness "/root/sftp-witness"))
                   (call-with-remote-output-file sftp-session witness
                                                 (cut display "hello" <>))
                   (call-with-remote-input-file sftp-session witness
                                                read)))))

            ;; Connect to the guest over SSH.  Make sure we can run commands
            ;; from the system profile.
            (test-equal "run executables from system profile"
              #t
              (call-with-connected-session/auth
               (lambda (session)
                 (let ((channel (make-channel session)))
                   (channel-open-session channel)
                   (channel-request-exec
                    channel
                    (string-append
                     "mkdir -p /root/.guix-profile/bin && "
                     "touch /root/.guix-profile/bin/path-witness && "
                     "chmod 755 /root/.guix-profile/bin/path-witness"))
                   (zero? (channel-get-exit-status channel))))))

            ;; Connect to the guest over SSH.  Make sure we can run commands
            ;; from the user profile.
            (test-equal "run executable from user profile"
              #t
              (call-with-connected-session/auth
               (lambda (session)
                 (let ((channel (make-channel session)))
                   (channel-open-session channel)
                   (channel-request-exec channel "path-witness")
                   (zero? (channel-get-exit-status channel))))))

            (test-end)
            (exit (= (test-runner-fail-count (test-runner-current)) 0))))))

  (gexp->derivation name test))

(define %test-openssh
  (system-test
   (name "openssh")
   (description "Connect to a running OpenSSH daemon.")
   (value (run-ssh-test name
                        ;; Allow root logins with an empty password to
                        ;; simplify testing.
                        (service openssh-service-type
                                 (openssh-configuration
                                  (permit-root-login #t)
                                  (allow-empty-passwords? #t)))
                        "/var/run/sshd.pid"
                        #:sftp? #t))))

(define %test-dropbear
  (system-test
   (name "dropbear")
   (description "Connect to a running Dropbear SSH daemon.")
   (value (run-ssh-test name
                        (service dropbear-service-type
                                 (dropbear-configuration
                                  (root-login? #t)
                                  (allow-empty-passwords? #t)))
                        "/var/run/dropbear.pid"

                        ;; XXX: Our Dropbear is not built with PAM support.
                        ;; Even when it is, it seems to ignore the PAM
                        ;; 'session' requirements.
                        #:test-getlogin? #f))))
