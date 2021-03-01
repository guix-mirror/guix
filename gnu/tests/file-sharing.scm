;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Simon South <simon@simonsouth.net>
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

;;;
;;; The Transmission Daemon service.
;;;

(define-module (gnu tests file-sharing)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu services)
  #:use-module (gnu services file-sharing)
  #:use-module (gnu services networking)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:export (%test-transmission-daemon))

(define %transmission-daemon-user "transmission")
(define %transmission-daemon-group "transmission")

(define %transmission-daemon-config-dir "/var/lib/transmission-daemon")
(define %transmission-daemon-watch-dir
  (string-append %transmission-daemon-config-dir "/watch"))
(define %transmission-daemon-incomplete-dir
  (string-append %transmission-daemon-config-dir "/incomplete"))

(define %transmission-daemon-settings-file
  (string-append %transmission-daemon-config-dir "/settings.json"))

(define %transmission-daemon-peer-port 51000) ; default is 51413

(define %transmission-daemon-rpc-port 9999)   ; default is 9091
(define %transmission-daemon-rpc-username "test-username")
(define %transmission-daemon-rpc-password "test-password")

(define %transmission-daemon-test-configuration
  (transmission-daemon-configuration
   (incomplete-dir-enabled? #t)
   (incomplete-dir %transmission-daemon-incomplete-dir)

   (watch-dir-enabled? #t)
   (watch-dir %transmission-daemon-watch-dir)

   (peer-port-random-on-start? #f)
   (peer-port %transmission-daemon-peer-port)

   (rpc-enabled? #t)
   (rpc-port %transmission-daemon-rpc-port)
   (rpc-whitelist-enabled? #f)
   (rpc-authentication-required? #t)
   (rpc-username %transmission-daemon-rpc-username)
   (rpc-password (transmission-password-hash %transmission-daemon-rpc-password
                                             "yEK0q3.X"))))

(define (run-transmission-daemon-test)
  (define os
    (marionette-operating-system
     (simple-operating-system
      (service dhcp-client-service-type)
      (service transmission-daemon-service-type
               %transmission-daemon-test-configuration))
     #:imported-modules '((gnu services herd)
                          (json parser))
     #:requirements '(transmission-daemon)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette
             (list #$(virtual-machine
                      (operating-system os)
                      (port-forwardings
                       `((9091 . ,%transmission-daemon-rpc-port)))))))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "transmission-daemon")

          ;; Make sure the "transmission" user and group have been created.
          (test-assert "\"transmission\" user exists"
            (marionette-eval
             '(begin
                (getpwnam #$%transmission-daemon-user)
                #t)
             marionette))
          (test-assert "\"transmission\" group exists"
            (marionette-eval
             '(begin
                (getgrnam #$%transmission-daemon-group)
                #t)
             marionette))

          ;; Make sure Transmission Daemon's configuration directory has been
          ;; created with the correct ownership and permissions.
          (test-assert "configuration directory exists"
            (marionette-eval
             '(eq? (stat:type (stat #$%transmission-daemon-config-dir))
                   'directory)
             marionette))
          (test-assert "configuration directory has correct ownership"
            (marionette-eval
             '(let ((config-dir (stat #$%transmission-daemon-config-dir))
                    (transmission-user (getpwnam #$%transmission-daemon-user)))
                (and (eqv? (stat:uid config-dir)
                           (passwd:uid transmission-user))
                     (eqv? (stat:gid config-dir)
                           (passwd:gid transmission-user))))
             marionette))
          (test-assert "configuration directory has expected permissions"
            (marionette-eval
             '(eqv? (stat:perms (stat #$%transmission-daemon-config-dir))
                    #o750)
             marionette))

          ;; Make sure the incomplete-downloads and watch directories have been
          ;; created with the correct ownership and permissions.
          (test-assert "incomplete-downloads directory exists"
            (marionette-eval
             '(eq? (stat:type (stat #$%transmission-daemon-incomplete-dir))
                   'directory)
             marionette))
          (test-assert "incomplete-downloads directory has correct ownership"
            (marionette-eval
             '(let ((incomplete-dir
                     (stat #$%transmission-daemon-incomplete-dir))
                    (transmission-user
                     (getpwnam #$%transmission-daemon-user)))
                (and (eqv? (stat:uid incomplete-dir)
                           (passwd:uid transmission-user))
                     (eqv? (stat:gid incomplete-dir)
                           (passwd:gid transmission-user))))
             marionette))
          (test-assert
              "incomplete-downloads directory has expected permissions"
            (marionette-eval
             '(eqv? (stat:perms (stat #$%transmission-daemon-incomplete-dir))
                    #o750)
             marionette))

          (test-assert "watch directory exists"
            (marionette-eval
             '(eq? (stat:type (stat #$%transmission-daemon-watch-dir))
                   'directory)
             marionette))
          (test-assert "watch directory has correct ownership"
            (marionette-eval
             '(let ((watch-dir (stat #$%transmission-daemon-watch-dir))
                    (transmission-user (getpwnam #$%transmission-daemon-user)))
                (and (eqv? (stat:uid watch-dir)
                           (passwd:uid transmission-user))
                     (eqv? (stat:gid watch-dir)
                           (passwd:gid transmission-user))))
             marionette))
          (test-assert "watch directory has expected permissions"
            (marionette-eval
             '(eqv? (stat:perms (stat #$%transmission-daemon-watch-dir))
                    #o770)
             marionette))

          ;; Make sure the settings file has been created and appears valid.
          (test-assert "settings file exists"
            (marionette-eval
             '(file-exists? #$%transmission-daemon-settings-file)
             marionette))
          (test-assert "settings file is valid JSON"
            (marionette-eval
             '(begin
                (use-modules (json parser))
                (with-input-from-file #$%transmission-daemon-settings-file
                  (lambda ()
                    (json->scm)))
                #t)
             marionette))
          (test-assert "settings file contains a non-empty JSON object"
            (marionette-eval
             '(begin
                (use-modules (json parser)
                             (srfi srfi-1))
                (let ((settings (with-input-from-file
                                    #$%transmission-daemon-settings-file
                                  (lambda ()
                                    (json->scm)))))
                  (and (list? settings)
                       (not (null? settings))
                       (every pair? settings))))
             marionette))

          ;; Make sure Transmission Daemon is running.
          (test-assert "transmission-daemon is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (live-service-running
                 (find (lambda (live-service)
                         (memq 'transmission-daemon
                               (live-service-provision live-service)))
                       (current-services))))
             marionette))

          ;; Make sure the daemon is listening for peer connections.
          (test-assert "transmission-daemon is listening for peers"
            (wait-for-tcp-port #$%transmission-daemon-peer-port marionette))

          ;; Make sure the daemon is listening for RPC-client connections.
          (test-assert "transmission-daemon is listening for RPC clients"
            (wait-for-tcp-port #$%transmission-daemon-rpc-port marionette))

          ;; Make sure the RPC-authentication settings are honored.
          (test-assert "transmission-daemon requires RPC authentication"
            (let ((transmission-remote
                   (string-append #+transmission "/bin/transmission-remote")))
              (with-error-to-port (%make-void-port "w")
                (lambda ()
                  (not (zero? (system* transmission-remote
                                       "--session-info")))))))
          (test-assert "transmission-daemon rejects incorrect RPC credentials"
            (let ((transmission-remote
                   (string-append #+transmission "/bin/transmission-remote"))
                  (wrong-auth-string
                   (string-append #$%transmission-daemon-rpc-username
                                  ":"
                                  "wrong-"
                                  #$%transmission-daemon-rpc-password)))
              (with-error-to-port (%make-void-port "w")
                (lambda ()
                  (not (zero? (system* transmission-remote
                                       "--auth" wrong-auth-string
                                       "--session-info")))))))
          (test-assert "transmission-daemon accepts correct RPC credentials"
            (let ((transmission-remote
                   (string-append #+transmission "/bin/transmission-remote"))
                  (auth-string
                   (string-append #$%transmission-daemon-rpc-username
                                  ":"
                                  #$%transmission-daemon-rpc-password)))
              (with-output-to-port (%make-void-port "w")
                (lambda ()
                  (zero? (system* transmission-remote
                                  "--auth" auth-string
                                  "--session-info"))))))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "transmission-daemon-test" test))

(define %test-transmission-daemon
  (system-test
   (name "transmission-daemon")
   (description "Test a running Transmission Daemon service.")
   (value (run-transmission-daemon-test))))
